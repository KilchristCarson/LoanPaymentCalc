Loan Payment Calculator program is an Ada-based program designed to compute loan payments and generate a detailed report for users, both on-screen and in a text file.
The CalcMonthlyPayment function determines the fixed monthly payment based on the principal, yearly interest rate, loan duration, and billing frequency per year.
Additionally, the CalcInterestPayment procedure calculates the interest portion of each payment, ensuring that interest is correctly deducted from the remaining balance over time.
The CalcAndWriteReport procedure orchestrates the full loan amortization schedule by iterating through the payment periods, computing principal and interest portions, and displaying them in a structured table format.
It prints the details to both the console and a text file for easy record-keeping. 
The program accounts for end-of-year summaries to enhance readability and ensures that the final payment correctly adjusts to pay off the loan without overpaying.
The package specification, LoanPayCalc.ads, defines the LoanRecord type, which stores user-provided loan details such as total principal, interest rate, loan term, and billing frequency.
The UseLoanPayCalc program serves as the main entry point, prompting the user for loan details and storing them in a LoanRecord.
It then calls CalcAndWriteReport to generate the amortization schedule and save it to a file.
This was a project from my Fall 2024 semester class, with some of the design choices being made due to certain restrictions I had to maintain.
