C entry2.f: example of a function with entry points, not all having
C the same interface.  This program is standard conforming although
C the result of FHALF is assigned to F2 since the result variables
C are equivalent.
       REAL FUNCTION F2 ( X ) 
       F2 = 2.0 * X 
       RETURN 

       ENTRY F3 ( X, Y ) 
       F3 = 3.0 * X * Y
       RETURN 

       ENTRY FHALF ( X ) 
       F2 = X / 2.0 
       RETURN 
       END 

      PROGRAM P
      REAL Y
      Y = F2(2.0)
      PRINT *, Y
      Y = F3(2.0,3.0)
      PRINT *, Y
      Y = FHALF(2.0)
      PRINT *, Y
      END
