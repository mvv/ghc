test('determ015',
     extra_clean(['A.o', 'A.hi', 'A.normal.hi']),
     run_command,
     ['$MAKE -s --no-print-directory determ015'])
