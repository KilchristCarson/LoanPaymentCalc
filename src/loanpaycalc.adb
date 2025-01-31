with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body LoanPayCalc is
   --The function 'CalcMonthlyPayment' generates a monthly payment with the initial principal, yearly interest rate,
   --the total years that the loan is active, and how often the customer will be billed per year.
   function CalcMonthlyPayment(Principal: Float; Interest: Float; Years: Integer; BillPY: Integer) return Float is
      MonthlyPayment: Float;
   begin
      MonthlyPayment := (interest / Float(BillPY) * Principal * (1.0 + interest / Float(BillPY)) ** (Years * BillPY)) /
                  ((1.0 + interest / Float(BillPY)) ** (Years * BillPY) - 1.0);
      return MonthlyPayment;
   end CalcMonthlyPayment;

   --The procedure 'CalcMonthlyPayment' uses the principal, yearly interest rate, and amount of times billed per year to compute
   --the amount of interest paid per payment, which is put into the out variable 'InterestPay'. This value updates as the loan decreases.
   procedure CalcInterestPayment(Principal: in Float; Interest: in Float; BillPY: in Integer; InterestPay: out Float) is
   begin
      InterestPay := Principal * (Interest / Float(BillPY));
   end CalcInterestPayment;

   --The procedure 'CalcAndWriteReport' uses a file name, principal, yearly interest rate, years of the loan, and amount of time billed per year and
   --also uses the prior function and procedure to calculate all the numbers necessary for the report, and prints them in a chart to make the product more
   --readable and user-friendly, as well as writing it to a text file, allowing for printable files customers can take home.

   procedure CalcAndWriteReport(File_Name:in String; TotalPrincipal:in Float;YearlyInterestRate:in Float;LoanYears:in Integer;BillPY: in Integer) is

      File : File_Type;
      NewBalance, iPayment, pPayment, mPayment, TotalInterestPaid: float;
      principal:float;

   begin
      Create(File,Out_File, File_Name);
      --This prints the output onto the screen and then saves a copy into the text file right after, that way the computations can be done at one time
      --and the file saves both copies at the same time with each iteration.
      put_Line("                                 FULL LOAN REPORT                                    ");
      Put_line("--------------------------------------------------------------------------------------");
      Put_Line("| Period:   Balance:        Payment:        Towards Loan:   Interest:     New Balance: |");
      Put_Line("--------------------------------------------------------------------------------------");

      put_Line(File,"                                 FULL LOAN REPORT                                    ");
      Put_line(File,"--------------------------------------------------------------------------------------");
      Put_Line(File,"| Period:   Balance:        Payment:        Towards Loan:   Interest:     New Balance: |");
      Put_Line(File,"--------------------------------------------------------------------------------------");


      mPayment := CalcMonthlyPayment(TotalPrincipal, YearlyInterestRate, LoanYears, BillPY);
      principal := TotalPrincipal;
      TotalInterestPaid := 0.0;

   for i in 1..(LoanYears * BillPY) loop
      CalcInterestPayment(principal, YearlyInterestRate, BillPY, iPayment);
      TotalInterestPaid := TotalInterestPaid + IPayment;
      pPayment := mPayment - iPayment;
      NewBalance := principal - PPayment;

         --this if statement creates an exception for when the final payment is too low and needs to be adjusted to match how much is left in the loan.
         --It also makes sure the principal payment is updated as well, leading to a fully-paid loan that doesn't go negative.
      if principal <= MPayment then
         MPayment := Principal;
         PPayment := Principal - IPayment;
         NewBalance := 0.0;
         end if;

      --both versions of each variable are printed one after the other, with the first being the screen output, and the second being the file output.
      put("   ");
      put(file,"   ");
      Put(i, 4); Put("    | $");
      Put(file,i, 4); Put(file,"    | $");
      Put(Principal, 6, 2, 0);
      Put(file,Principal, 6, 2, 0);
      Put("    | $");
      Put(file,"    | $");
      Put(mPayment, 6, 2, 0);
      Put(file,mPayment, 6, 2, 0);
      Put("    | $");
      Put(file,"    | $");
      Put(pPayment, 6, 2, 0);
      Put(file,pPayment, 6, 2, 0);
      Put("    | $");
      Put(file,"    | $");
      Put(iPayment, 6, 2, 0);
      Put(file,iPayment, 6, 2,0);
      Put("    | $");
      Put(file,"    | $");
      Put(NewBalance, 6, 2, 0);
      Put(file,NewBalance, 6, 2, 0);
      New_Line;
      New_Line(file);

         --This if statement allows for the inclusion of an 'end of year' message after every year in the output.
         --Having this feature gives more readability to the customer and will change depending on how many times
         --the customer chooses to be billed per year.
         if (i mod BillPY = 0) then
           put_Line("End of Year" & Integer'Image(i/BillPY));
           put_Line(file,"End of Year " & Integer'Image(i/BillPY));
         end if;
      --the final value for principal is changed to make sure the calculations for the next iteration are done correctly.
      Principal := NewBalance;
   end loop;
   --This displays the total interest paid to the customer after the calculations are complete.
   put_Line("---------------------------------------------------------------------------------------");
   Put("   Total Interest Paid: $");put(TotalInterestPaid,4,2,0);New_Line;
   Put_Line("---------------------------------------------------------------------------------------");

   put_Line(file,"---------------------------------------------------------------------------------------");
   Put(file,"   Total Interest Paid: $");put(file,TotalInterestPaid,4,2,0);New_Line(file);
   Put_Line(file,"---------------------------------------------------------------------------------------");


   Close(File);
   end CalcAndWriteReport;
end LoanPayCalc;
