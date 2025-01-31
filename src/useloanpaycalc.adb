--UseLoanPayCalc.adb
--The file calls for input from the consumer and records the input into LoanRecord
--The input is then used in the procedure CalcAndWriteReport to calculate and print the
--report not only on the screen but it also writes it into a sequential text file.
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with LoanPayCalc; use LoanPayCalc;

procedure UseLoanPayCalc is

   LoanR: LoanRecord;

begin
   Put("What is the total loan amount/principal? "); Get(LoanR.TotalPrincipal);
   Put("What is the yearly interest rate for the loan(decimal form)? "); Get(LoanR.YearlyInterestRate);
   Put("How long is the loan in years? "); Get(LoanR.LoanYears);
   Put("How often will you be getting billed per year? "); Get(LoanR.BilledPerYear);
   --Allowing users to input both the amount of years and the amount of times billed per year creates more
   --flexibility on what type of loans customers have at their disposal, whether monthly, quarterly, yearly, etc.

   CalcAndWriteReport(".\LoanReport.txt", LoanR.TotalPrincipal, LoanR.YearlyInterestRate,
                      LoanR.LoanYears, LoanR.BilledPerYear);
end UseLoanPayCalc;
