package pushka

/**
 * @param leanOptions When false, `None` writes as `null`.
 *                    Otherwise it doesn't create property.
 *                    Switched on by default.
 */
case class Config(leanOptions: Boolean)

object Config {
  def default = Config(
    leanOptions = true
  )
}
