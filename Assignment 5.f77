        PROGRAM lastassignment
                INTEGER array(999), length, I, target
                PRINT *, 'Welcome! This program runs indefintely.'
                PRINT *, 'CTRL + C to exit.'
                PRINT *, 'Enter the length of the desired array.'
                READ *, length
                DO 10, I = 1, length
                      PRINT *, 'Enter the ', I, ' number.'
                      READ *, num
                      array(I) = num
10              CONTINUE

                CALL SORT (array, length)
15              PRINT *, 'What number are you searching for?'
                READ *, target
                CALL SEARCH (array, length, target)
                GOTO 15
        END
        
        SUBROUTINE SORT (A, N)
        INTEGER N, A(N), TEMP, K, L
        DO 11 K = 1, N - 1
           DO 12 L = K+1, N
              IF (A(K).GT.A(L)) THEN
                 TEMP = A(K)
                 A(K) = A(L)
                 A(L) = TEMP
              ENDIF
12         CONTINUE
11      CONTINUE
        RETURN
        END
        
        SUBROUTINE PRNAR (A, N)
        INTEGER N, A(N), count
        DO 13 count = 1, N
           PRINT *, A(count)
13      CONTINUE

        END
        
        SUBROUTINE SEARCH (A, N, target)
        INTEGER N, A(N), target, low, mid, high
        LOGICAL found
        low = 1
        high = N
        mid = high / 2
        found = .FALSE.
14        IF (.NOT. found .AND. low .LE. high) THEN
             IF (A(mid) .EQ. target) THEN
                found = .TRUE.
                target = A(mid)
             ELSEIF (target .LT. A(mid)) THEN
                high = mid - 1
                mid = (low + high) / 2
             ELSE
                 low = mid + 1
                 mid = (low + high) / 2
             ENDIF
             GOTO 14
            ENDIF
           IF (found) THEN
              PRINT *, 'Your number has been found!'
           ELSE
              PRINT *, 'Your number could not be found.'
           ENDIF
        END
             
                 
