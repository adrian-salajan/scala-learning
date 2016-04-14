package ro.asalajan.ep.version1

import ro.asalajan.ep.Employee

//can't process new Contractor type without Payroll modification which will cause rebuild
object Version1 {

  trait Payroll {
    def processEmployees(employees: Vector[Employee]): Either[String, Throwable]
  }

  class USPayroll extends Payroll {
    override def processEmployees(employees: Vector[Employee]): Either[String, Throwable] = {
      employees.foreach(e => println(s"US-payroll:$e"))
      new Left("US done")
    }
  }
  class CanadaPayroll extends Payroll {
    override def processEmployees(employees: Vector[Employee]): Either[String, Throwable] = {
      employees.foreach(e => println(s"Canada-payroll:$e"))
      new Left("Canada done")
    }
  }

  //easy to process Japan employees
  class JapanPayroll extends Payroll {
    override def processEmployees(employees: Vector[Employee]): Either[String, Throwable] = {
      employees.foreach(e => println(s"Japan-payroll:$e"))
      new Left("Japan done")
    }
  }

  //can't process Contractors without modifications to Payroll, and rebuild

  def main(args: Array[String]): Unit = {
    val e1 = Employee("1", 1)
    val e2 = Employee("2", 2)

    val emps = Vector[Employee](e1, e2)


    new USPayroll().processEmployees(emps)
    new CanadaPayroll().processEmployees(emps)
    new JapanPayroll().processEmployees(emps)
  }


}
