package models.jnlp

protected[jnlp] object SigningConfig {

  private lazy val PlayConfig = play.Play.application.configuration()

  lazy val KeyName      = PlayConfig.getString("jarsigner.keyname")
  lazy val KeystorePass = PlayConfig.getString("jarsigner.keystorepass")
  lazy val KeyPass      = PlayConfig.getString("jarsigner.keypass")

}
