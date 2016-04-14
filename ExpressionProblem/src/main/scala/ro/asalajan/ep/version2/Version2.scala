package ro.asalajan.ep.version2

import ro.asalajan.ep.{Contractor, Employee}

//can't add JapanPayroll without modifications to PayrollVisitor
object Version2 {
  val e1 = Employee("employee", 1)
  val e2 = Employee("employee", 2)

  val employees = Vector[Employee](e1, e2)

  case class USPayroll(emps: Vector[Employee]) {
    def accept(v: PayrollVisitor) = v.visit(this);
  }

  case class CanadaPayroll(emps: Vector[Employee], conts: Vector[Contractor]) {
    def accept(v: PayrollVisitor) = v.visit(this);
  }

  trait PayrollVisitor {
    def visit(payroll: USPayroll): Either[String, Throwable]
    def visit(payroll: CanadaPayroll): Either[String, Throwable]
  }

  class EmployeePayrollVisitor() extends PayrollVisitor {
    override def visit(payroll: USPayroll): Either[String, Throwable] = {
      payroll.emps.foreach( e => println(s"us-payroll-$e"))
      Left("us-done")
    }

    override def visit(payroll: CanadaPayroll): Either[String, Throwable] = {
      payroll.emps.foreach( e => println(s"canada-payroll-$e"))
      Left("canada-done")
    }
  }

  //lets process contractors in Canada
  class ContractorPayrollVisitor extends PayrollVisitor {
    override def visit(payroll: USPayroll): Either[String, Throwable] = {
      Left("no contractors in us")
    }

    override def visit(payroll: CanadaPayroll): Either[String, Throwable] = {
      payroll.conts.foreach( e => println(s"canada-payroll-$e"))
      Left("canada-contractors-done")
    }
  }

  //can't add JapanPayroll without modifications to PayrollVisitor


  def main(args: Array[String]): Unit = {
    val conts = Vector[Contractor](Contractor("contractor 1"), Contractor("contractor 2"))

    new EmployeePayrollVisitor().visit(USPayroll(employees))
    new EmployeePayrollVisitor().visit(CanadaPayroll(employees, conts))

    new ContractorPayrollVisitor().visit(USPayroll(employees))
    new ContractorPayrollVisitor().visit(CanadaPayroll(employees, conts))

  }

}
