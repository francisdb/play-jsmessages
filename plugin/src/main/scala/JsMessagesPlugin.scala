package jsmessages.plugin

import sbt._
import sbt.Keys._
import play.Keys._
import play.PlayExceptions.AssetCompilationException
import play.PlaySourceGenerators
import play.api.i18n.MessagesPortal
import jsmessages.api.JsMessages

object JsMessagesPlugin extends Plugin with JsmessagesAssetCompiler{
  val jsmessagesEntryPoints = SettingKey[PathFinder]("play-jsmessages-entry-points")
  val jsmessagesOptions = SettingKey[Seq[String]]("play-jsmessages-options")

  val watch = { file:File => (file ** "messages*") }
  val naming = { (name:String, min:Boolean) =>
    if (min){
      s"$name.min.js"
    }else{
      s"$name.js"
    }
  }
  val compile = { (file:File, options:Seq[String]) => JsmessagesCompiler.compile(file, options) }

  val jsmessagesWatcher = AssetsCompiler("jsmessages", watch, jsmessagesEntryPoints, naming, compile, jsmessagesOptions)

  val jsmessagesSettings = Seq(
    jsmessagesEntryPoints <<= (confDirectory in Compile)(base => (base ** "messages*")),
    jsmessagesOptions := Seq.empty[String],
    resourceGenerators in Compile <+= jsmessagesWatcher
  )
}

object JsmessagesCompiler{
  def compile(messageFile: File, opts: Seq[String]): (String, Option[String], Seq[File]) = {

    println("compiling " + messageFile)

    val namespace = None
    val lang = "???"

    val messages = Map( lang ->
      MessagesPortal.parseMessagesFile(messageFile)
    )

    val js = new JsMessages(messages).all(namespace)

    val debug = js.body

    // TODO do we want to provide a minified version?
    val min = None
    val dependencies = Seq.empty

    (debug, min, dependencies)
  }
}

/**
 * Copy from play2 because we need to support conf folder assets
 */
trait JsmessagesAssetCompiler{
  // Name: name of the compiler
  // files: the function to find files to compile from the assets directory
  // naming: how to name the generated file from the original file and whether it should be minified or not
  // compile: compile the file and return the compiled sources, the minified source (if relevant) and the list of dependencies
  def AssetsCompiler(name: String,
                     watch: File => PathFinder,
                     filesSetting: sbt.SettingKey[PathFinder],
                     naming: (String, Boolean) => String,
                     compile: (File, Seq[String]) => (String, Option[String], Seq[File]),
                     optionsSettings: sbt.SettingKey[Seq[String]]) =
    (state, confDirectory in Compile, resourceManaged in Compile, cacheDirectory, optionsSettings, filesSetting, requireJs) map { (state, conf, resources, cache, options, files, requireJs) =>

      println("checking for changes...")

      val requireSupport = if (!requireJs.isEmpty) {
        Seq("rjs")
      } else Seq[String]()

      val cacheFile = cache / name

      val currentInfos = watch(conf).get.map(f => f -> FileInfo.lastModified(f)).toMap

      val (previousRelation, previousInfo) = Sync.readInfo(cacheFile)(FileInfo.lastModified.format)

      if (previousInfo != currentInfos) {

        //a changed file can be either a new file, a deleted file or a modified one
        lazy val changedFiles: Seq[File] = currentInfos.filter(e => !previousInfo.get(e._1).isDefined || previousInfo(e._1).lastModified < e._2.lastModified).map(_._1).toSeq ++ previousInfo.filter(e => !currentInfos.get(e._1).isDefined).map(_._1).toSeq

        println("Changed: " + changedFiles)

        //erease dependencies that belong to changed files
        val dependencies = previousRelation.filter((original, compiled) => changedFiles.contains(original))._2s
        dependencies.foreach(IO.delete)

        /**
         * If the given file was changed or
         * if the given file was a dependency,
         * otherwise calculate dependencies based on previous relation graph
         */
        val generated: Seq[(File, java.io.File)] = (files pair relativeTo(Seq(conf))).flatMap {
          case (sourceFile, name) => {
            if (changedFiles.contains(sourceFile) || dependencies.contains(new File(resources, "public/javascripts/" + naming(name, false)))) {
              val (debug, min, dependencies) = try {
                compile(sourceFile, options ++ requireSupport)
              } catch {
                case e: AssetCompilationException => throw PlaySourceGenerators.reportCompilationError(state, e)
              }
              val out = new File(resources, "public/javascripts/" + naming(name, false))
              println("writing " + out)
              IO.write(out, debug)
              (dependencies ++ Seq(sourceFile)).toSet[File].map(_ -> out) ++ min.map { minified =>
                val outMin = new File(resources, "public/javascripts/" + naming(name, true))
                IO.write(outMin, minified)
                (dependencies ++ Seq(sourceFile)).map(_ -> outMin)
              }.getOrElse(Nil)
            } else {
              previousRelation.filter((original, compiled) => original == sourceFile)._2s.map(sourceFile -> _)
            }
          }
        }

        //write object graph to cache file
        Sync.writeInfo(cacheFile,
          Relation.empty[File, File] ++ generated,
          currentInfos)(FileInfo.lastModified.format)

        // Return new files
        generated.map(_._2).distinct.toList

      } else {
        // Return previously generated files
        previousRelation._2s.toSeq
      }

    }
}