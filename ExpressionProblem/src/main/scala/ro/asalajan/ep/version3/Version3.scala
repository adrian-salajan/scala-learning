package ro.asalajan.ep.version3

import ro.asalajan.ep.version3.Version3.USContractorPayrollInstance
import ro.asalajan.ep.{Contractor, Employee}

object Version3 {

  val e1 = Employee("employee", 1)
  val e2 = Employee("employee", 2)

  val employees = Vector[Employee](e1, e2)

  trait PayrollSystem {
      type P <: Payroll

      trait Payroll {
        def processEmployees(employees: Vector[Employee]): Either[String, Throwable]
      }

    def processPayroll(payroll: P): Either[String, Throwable]
  }


  trait USPayrollSystem extends PayrollSystem {

    class USPayroll extends Payroll {
      override def processEmployees(employees: Vector[Employee]): Either[String, Throwable] = {
        employees.foreach(e => println(s"us-employee-$e"))
        Left("US-emps-done")
      }
    }
  }

  object USPayrollInstance extends USPayrollSystem {
    override type P = USPayroll

    override def processPayroll(payroll: USPayroll): Either[String, Throwable] = {
      payroll.processEmployees(employees)
    }
  }

  trait CanadaPayrollSystem extends PayrollSystem {

    class CanadaPayroll extends Payroll {
      override def processEmployees(employees: Vector[Employee]): Either[String, Throwable] = {
        employees.foreach(e => println(s"canada-employee-$e"))
        Left("Canada-emps-done")
      }
    }

  }

  object CanadaPayrollInstance extends CanadaPayrollSystem {
    override type P = CanadaPayroll

    override def processPayroll(payroll: CanadaPayroll): Either[String, Throwable] = {
      payroll.processEmployees(employees)
    }
  }

  //add japan payroll (1) step-by-step
  /*
  trait JapanPayrollSystem extends PayrollSystem {

    class JapanPayroll extends Payroll {
      override def processEmployees(employees: Vector[Employee]): Either[String, Throwable] = {
        employees.foreach(e => println(s"Japan-employee-$e"))
        Left("Japan-emps-done")
      }
    }
  }

  object JapanPayrollInstance extends  JapanPayrollSystem {
    override type P = JapanPayroll

    override def processPayroll(payroll: JapanPayroll): Either[String, Throwable] = {
      payroll.processEmployees(employees)
    }
  }
  */

  //add japan payroll (2) all-in one

  object JapanPayrollInstance extends PayrollSystem {
    override type P = JapanPayroll

    class JapanPayroll extends Payroll {
      override def processEmployees(employees: Vector[Employee]): Either[String, Throwable] = {
        employees.foreach(e => println(s"Japan-employee-$e"))
        Left("Japan-emps-done")
      }
    }

    override def processPayroll(payroll: JapanPayroll): Either[String, Throwable] = {
      payroll.processEmployees(employees)
    }
  }

  //contractors payroll

  trait ContractorPayrollSystem extends PayrollSystem {
    type P <: ContractorPayroll //redefines P from PayrollSystem, but keeps the original bounds P <: Payroll

    /* this shadows PayrollSystem.Payroll */
    trait ContractorPayroll extends super.Payroll {
      def processContractors(contractors: Vector[Contractor]): Either[String, Throwable]
    }
  }

  trait USContractorPayrollSystem extends USPayrollSystem with ContractorPayrollSystem {
    override type P = USPayrollWithContractors

    class USPayrollWithContractors extends super.USPayroll with ContractorPayroll {
      override def processContractors(contractors: Vector[Contractor]): Either[String, Throwable] = {
        contractors.foreach(c => println(s"us-contractor-$c"))
        Left("us-contractors-done")
      }
    }
  }

  object USContractorPayrollInstance extends USContractorPayrollSystem {
    override def processPayroll(payroll: USPayrollWithContractors): Either[String, Throwable] = {
      payroll.processEmployees(employees)
      payroll.processContractors(Vector[Contractor](Contractor("contractor-1"), Contractor("contractor-2")))
    }
  }


  def main(args: Array[String]): Unit = {
    val usPayroll = new USPayrollInstance.USPayroll
    USPayrollInstance.processPayroll(usPayroll)

    val canadaPayroll = new CanadaPayrollInstance.CanadaPayroll
    CanadaPayrollInstance.processPayroll(canadaPayroll)

    println("------------japan------")
    JapanPayrollInstance.processPayroll(new JapanPayrollInstance.JapanPayroll)

    println("---------us-with-contractors-------")
    USContractorPayrollInstance.processPayroll(new USContractorPayrollInstance.USPayrollWithContractors)
  }


}
