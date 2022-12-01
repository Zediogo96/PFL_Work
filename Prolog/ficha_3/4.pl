% a
print_n(_, 0).
print_n(Symbol, N) :-
    write(Symbol),
    NewN is N - 1,
    print_n(Symbol, NewN).


% b
print_string("").
print_string([Code | T]) :- char_code(Char, Code),
                            write(Char),
                            print_string(T).

print_text(Text, Symbol, Padding) :- write(Symbol),
                                     print_n(' ', Padding),
                                     print_string(Text),
                                     print_n(' ', Padding),
                                     write(Symbol).




