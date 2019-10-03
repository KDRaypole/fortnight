PROGRAM Fortnight
    IMPLICIT NONE
    PRINT *, "[ Fortnight ]"
	!PRINT *, CalculateSumOfEvenFibonnaci(4000000)

	CONTAINS

	INTEGER FUNCTION CalculateSumOfEvenFibonnaci(MAX)
		INTEGER, INTENT(IN) :: MAX
		INTEGER :: PreviousTerm = 1
		INTEGER :: CurrentTerm  = 2
		INTEGER :: NextTerm     = 0
		INTEGER :: Sum          = 2

		! Calculate next fibonacci while the current term is less than the max
		DO WHILE (CurrentTerm .LT. MAX)
			! Calculate the next term
			NextTerm = CurrentTerm + PreviousTerm
			! Add the new term if it's less than the max and divisible by 2
			IF (NextTerm .LE. MAX .AND. MOD(NextTerm, 2) .EQ. 0) THEN
				Sum = Sum + NextTerm
			END IF
			! Update the previous and current terms
			PreviousTerm = CurrentTerm
			CurrentTerm = NextTerm
		END DO

		! Return the sum
		CalculateSumOfEvenFibonnaci = Sum
	END FUNCTION CalculateSumOfEvenFibonnaci

END PROGRAM Fortnight