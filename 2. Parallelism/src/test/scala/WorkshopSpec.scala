import org.scalatest.funsuite.AnyFunSuite

// Тестовий клас, який перевіряє логіку з Workshop.scala
class WorkshopSpec extends AnyFunSuite:
  
  test("Функція categorize повинна правильно визначати рівні ризику"):
    assert(Workshop.categorize(90.0) == Workshop.HighRisk)
    assert(Workshop.categorize(60.0) == Workshop.MediumRisk)
    assert(Workshop.categorize(10.0) == Workshop.LowRisk)

  test("Функція getMultiplier повинна повертати правильні коефіцієнти"):
    assert(Workshop.getMultiplier(Workshop.HighRisk) == 1.5)
    assert(Workshop.getMultiplier(Workshop.MediumRisk) == 1.2)
    assert(Workshop.getMultiplier(Workshop.LowRisk) == 1.0)

