package main.chapter12

import java.util.Date

import main.Test

class TestValidation extends Test {

  def validName(name: String): Validation[String, String] =
    if (name != "") Success(name)
    else Failure("Name cannot be empty")

  def validBirthdate(birthdate: String): Validation[String, Date] =
    try {
      import java.text._
      Success(new SimpleDateFormat("yyyy-MM-dd").parse(birthdate))
    } catch {
      case _ => Failure("Birthdate must be in the form yyyy-MM-dd")
    }


  def validPhone(phoneNumber: String): Validation[String, String] =
    if (phoneNumber.matches("[0-9]{10}"))
      Success(phoneNumber)
    else Failure("Phone number must be 10 digits")

  case class WebForm(name: String, birthdate: Date, phoneNumber: String)
  def validWebForm(name: String,
                   birthdate: String,
                   phone: String): Validation[String, WebForm] =
    Validation.applic.map3(
      validName(name),
      validBirthdate(birthdate),
      validPhone(phone))(
      WebForm(_,_,_))


  "validation" should "acc errors" in {
    validWebForm("", "abc", "1234567890") shouldBe Failure("Name cannot be empty", Vector("Birthdate must be in the form yyyy-MM-dd"))
  }


}
