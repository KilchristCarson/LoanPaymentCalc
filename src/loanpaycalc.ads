--LoanPayCalc.ads
--This file is package specification for the calculations, storing loan
--details, and creating the report, both for on screen and the created text file.
package LoanPayCalc is
   
   --The record 'LoanRecord' allows the user input to be stored and used in the functions
   --CalcMonthlyPayment, CalcInterestPayment, and CalcAndWriteReport.
      type LoanRecord is
      record
         TotalPrincipal: Float;
         YearlyInterestRate: Float;
         LoanYears: Integer;
         BilledPerYear: Integer;
      end record;

   function CalcMonthlyPayment(Principal: Float; Interest: Float; Years: Integer; BillPY: Integer) return Float;

   procedure CalcInterestPayment(Principal: in Float; Interest: in Float; BillPY: in Integer; interestPay: out Float);
   
   procedure CalcAndWriteReport(File_Name: in String;TotalPrincipal: in Float;YearlyInterestRate: in Float;LoanYears: in Integer;BillPY: in Integer);

end LoanPayCalc;
