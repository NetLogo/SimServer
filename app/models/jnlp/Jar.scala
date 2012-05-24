package models.jnlp

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/24/12
 * Time: 4:43 PM
 */

abstract class AbstractJar(val jarName: String, val isMain: Boolean, val isLazy: Boolean)

class Jar(name: String, iAmLazy: Boolean) extends AbstractJar(name, false, iAmLazy)
class MainJar(name: String) extends AbstractJar(name, true, false)
