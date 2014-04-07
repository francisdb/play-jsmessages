package play.api.i18n

import java.io.File
import scalax.io.JavaConverters._

/**
 * Helper to allow us to get access to the play.api.i18n package
 */
object MessagesPortal {

  def parseMessagesFile(messageFile:File) = {
    new Messages.MessagesParser(messageFile.asInput, messageFile.toString).parse.map { message =>
      message.key -> message.pattern
    }.toMap
  }

}
