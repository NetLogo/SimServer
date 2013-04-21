package controllers

import
  play.api.mvc.Controller

import
  controllers.action.RestrictedAction

import
  models.hubnet.HubNetServerRegistry

/**
 * Created with IntelliJ IDEA.
 * User: jason
 * Date: 4/21/13
 * Time: 3:12 PM
 */

object SecureJNLP extends Controller {

  private val SecureDomains = Seq("http://modelsim.tech.northwestern.edu", "http://abmplus.tech.northwestern.edu")

  object HTTPParams {
    val TeacherNameKey = "teacher_name"
    val DataKey        = "secure_data"
  }

  def generateHubNetServerJNLP(teacherName: String) = RestrictedAction(SecureDomains) {

    implicit request =>

    import HTTPParams._

    val (modulus, exponent) = HubNetServerRegistry.registerTeacher(teacherName)
    val secureParams = Map(
      "netlogo.registry.http.teacher_name_key" -> TeacherNameKey,
      "netlogo.registry.http.data_key"         -> DataKey,
      "netlogo.registry.teacher_name"          -> teacherName,
      "netlogo.registry.register_url"          -> routes.HubNet.registerTeacherAddress.absoluteURL(),
      "netlogo.registry.public_key.modulus"    -> modulus,
      "netlogo.registry.public_key.exponent"   -> exponent
    )

    JNLP.handleJNLPGeneration(request, secureParams)

  }

}
