: none 0 ;
: share 1 ;
: exclusive 2 ;

\\ capability of acquiring a share lock
: lock_s exclusive != ;

\\ capability of acquiring an exclusive lock
: lock_x none = ;


: test
    none lock_s verify
    share lock_s verify
    exclusive lock_s not verify

    none lock_s verify
    share lock_x not verify
    exclusive lock_x not verify
;