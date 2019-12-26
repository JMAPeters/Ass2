cd "%~dp0"


start cmd /k stack run 55500 55501 55502
start cmd /k stack run 55501 55500 55502
start cmd /k stack run 55502 55500 55501 55503 55504
start cmd /k stack run 55503 55502 55505
start cmd /k stack run 55504 55502 55505
start cmd /k stack run 55505 55503 55504
start cmd /k stack run 55506 55507 55508
start cmd /k stack run 55507 55506 55508
start cmd /k stack run 55508 55506 55507 55509 55510
start cmd /k stack run 55509 55508 55511
start cmd /k stack run 55510 55508 55511
start cmd /k stack run 55511 55509 55510