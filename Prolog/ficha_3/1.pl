% CUT means !, which always succeeds but cannot be backtracked.
% It is used to prevent unwanted backtracking, for example, to prevent extra solutions being found by Prolog.
% The cut should be used sparingly. There is a temptation to insert cuts experimentally into code that is not working correctly. If you do this, bear in mind that when debugging is complete, you should understand the effect of, and be able to explain the need for, every cut you use. The use of a cut should thus be commented. 

%      [S(X), S(Y)]
%         /  \
%   x=1  /    \ x=2
%       /      \ 
%   [S(Y)]    [!S(Y)]
%     /  \        /  \
% y=1/    \y=2 y=1/    \y=2
%   /      \     /      \
% []      [!]   []     [!]


%      [S(X), !, S(Y)]
%         /  \
%   x=1  /    \ 
%       /      \ 
%   [!, S(Y)]   X
%     /  \    \
% y=1/    \y=2 \ X
%   /      \    
% [S(Y)]      [!]   

data(one).
data(two).
data(three).
cut_test_a(X):- data(X).
cut_test_a(‘five’).
cut_test_b(X):- data(X), !.
cut_test_b(‘five’).
cut_test_c(X, Y):- data(X), !, data(Y).
cut_test_c(‘five’, ‘five’).

% Prolog WRITE is defined as, ‘write’ is an in-built predicate in prolog, it returns all the information that we required to show in the output, it gives clear output that means if we required output related to the program and with something friendly then we can use write predicate in the program, it uses terms to the current output by using appropriate brackets and operators, and that can be called as ‘write(Y)’ in which it writes the term Y to the current output stream, the stream is a continuous window with code writer, if an argument is a string, then it prints the string without quotes.

% 2
% a)
% cut_test_a(X), write(X), nl, fail. => one, two, three, false.
% b)
% cut_test_b(X), write(X), nl, fail. => one, false.
% c)
% cut_test_c(X, Y), write(X-Y), nl, fail. 

% cause encontrar o primeiro data(X) que será 1, em depois leva cuta às procuras mais à esquerda, logo só continuara a procurar no ramo do primeiro data(X) = 1, que retorna one-one, one-two, one-three.

immature(X):- adult(X), !, fail.
% cut vermelho - impede immature(X). de correr, cortando para adultos e nao-adultos


