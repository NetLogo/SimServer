package controllers

import
  controllers.action.APIAction

import
  models.hubnet.UnsafeHubNetServerRegistry

/**
 * Created with IntelliJ IDEA.
 * User: jason
 * Date: 4/30/14
 * Time: 1:26 PM
 */

// It's saddening that the project has reached the point where this is necessary
object UnsafeJNLP {

  object HTTPParams {
    val TeacherNameKey = "teacher_name"
    val DataKey        = "data"
  }

  def generateHubNetServerJNLP(teacherName: String) = APIAction {

    implicit request =>

    import HTTPParams._

    UnsafeHubNetServerRegistry.registerTeacher(teacherName)

    val registryParams = Map(
      "netlogo.registry.http.teacher_name_key" -> TeacherNameKey,
      "netlogo.registry.http.data_key"         -> DataKey,
      "netlogo.registry.teacher_name"          -> teacherName,
      "netlogo.registry.register_url"          -> routes.HubNet.unsafeRegisterTeacherAddress.absoluteURL()
    )

    JNLP.handleJNLPGeneration(request, registryParams)

  }

}
