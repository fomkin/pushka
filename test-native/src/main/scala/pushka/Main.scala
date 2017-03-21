package pushka

object Main {
  def main(args: Array[String]): Unit = {
    CaseClassSpec.run
    SealedTraitSpec.run
    DefaultRWSpec.run
  }
}
