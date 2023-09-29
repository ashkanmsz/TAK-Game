
%------- Facts -------------------------------

neighbour(1,2).
neighbour(1,5).

neighbour(2,1).
neighbour(2,3).
neighbour(2,6).

neighbour(3,2).
neighbour(3,4).
neighbour(3,7).

neighbour(4,8).
neighbour(4,3).

neighbour(5,1).
neighbour(5,6).
neighbour(5,9).

neighbour(6,5).
neighbour(6,2).
neighbour(6,7).
neighbour(6,10).

neighbour(7,6).
neighbour(7,3).
neighbour(7,8).
neighbour(7,11).

neighbour(8,7).
neighbour(8,4).
neighbour(8,12).

neighbour(9,5).
neighbour(9,10).
neighbour(9,13).

neighbour(10,9).
neighbour(10,6).
neighbour(10,11).
neighbour(10,14).

neighbour(11,7).
neighbour(11,10).
neighbour(11,12).
neighbour(11,15).

neighbour(12,8).
neighbour(12,11).
neighbour(12,16).

neighbour(13,9).
neighbour(13,14).

neighbour(14,13).
neighbour(14,15).
neighbour(14,10).

neighbour(15,14).
neighbour(15,11).
neighbour(15,16).

neighbour(16,12).
neighbour(16,15).

% --------------------------------------------------------

in_same_direct(N,H1,0,0):-
			   neighbour(N,H1),!.

in_same_direct(N,H1,H2,0):-
			   neighbour(N,H1),
			   neighbour(H1,H2),
			   S1 is H1-N,
			   S2 is H2-H1,
			   S1 == S2,!.


in_same_direct(N,H1,H2,H3):-
			   neighbour(N,H1),
			   neighbour(H1,H2),
			   neighbour(H2,H3),
			   S1 is H1-N,
			   S2 is H2-H1,
			   S3 is H3-H2,
			   S1 == S2,
			   S2 == S3,!.

% ---------- add an element to one list---------------------------------

addfirst1(X,L,[X|L]).

%------------add a list to another list---------------------------------

addfirst([],L,L).

addfirst([H|T],L,[H|T3]):-
			addfirst(T,L,T3),!.

%------- push(I,L,L2,L3)----- L[I]=L2 ----------------------------------

push(I,L,L2,L3):-
	push2(I,1,L,L3,L2).

push2(I,I,[_|T],[L2|T],L2).

push2(I,J,[H|T],[H|T3],L2):-
	I\=J,
	J2 is J+1,
	push2(I,J2,T,T3,L2).


%-----------------------------------------------------------

rev(L,L2):-
	rev2(L,[],L2).

rev2([],L,L).


rev2([H|T],L2,L3):-
	rev2(T,[H|L2],L3).


% --- winning conditions ---------------------------

win(Brd, Plyr)  :-
                     (win_check(Brd, Plyr,1,1,[]),!);
                     (win_check(Brd, Plyr,2,2,[]),!);
                     (win_check(Brd, Plyr,3,3,[]),!);
                     (win_check(Brd, Plyr,4,4,[]),!);
                     (win_check(Brd, Plyr,5,5,[]),!);
                     (win_check(Brd, Plyr,9,9,[]),!);
                     (win_check(Brd, Plyr,13,13,[]),!).


% -------- win check -----------------------------------

win_check(_,_,Src,Nsrc,_):- % Vertical

              (Src==1;Src==2;Src==3;Src==4),
              (Nsrc==13;Nsrc==14;Nsrc==15;Nsrc==16),!.

win_check(_,_,Src,Nsrc,_):- % Horizontal

              (Src==1;Src==5;Src==9;Src==13),
              (Nsrc==4;Nsrc==8;Nsrc==12;Nsrc==16),!.

win_check(Brd,Plyr,Src,Nsrc,L):-
                 el(Src,Brd,[H2|_]),
                 H2==Plyr,           % oon khune bayad w bashe na b
                 neighbour(Nsrc,Dist),

                 addfirst1(Nsrc,L,L2),
                 not(member(Dist,L2)),

                 el(Dist,Brd,[H|_]),
                 H==Plyr,	    % hamsayat ham bayad w bashe
                 win_check(Brd,Plyr,Src,Dist,L2),!.


% -----AI moves -----------------------------------------

%1
b_move([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], Plyr, [A2,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P],Y):-               % Plyr = b
                                                                                       length(A,X),
                                                                                       X<9,
                                                                                       addfirst1(Plyr,A,A2),
                                                                                       Y = 1.
%2
b_move([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], Plyr, [A,B2,C,D,E,F,G,H,I,J,K,L,M,N,O,P],Y):-
                                                                                       length(B,X),
                                                                                       X<9,
                                                                                       addfirst1(Plyr,B,B2),
                                                                                       Y = 2.
%3
b_move([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], Plyr, [A,B,C2,D,E,F,G,H,I,J,K,L,M,N,O,P],Y):-
                                                                                       length(C,X),
                                                                                       X<9,
                                                                                       addfirst1(Plyr,C,C2),
                                                                                       Y = 3.
%4
b_move([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], Plyr, [A,B,C,D2,E,F,G,H,I,J,K,L,M,N,O,P],Y):-
                                                                                       length(D,X),
                                                                                       X<9,
                                                                                       addfirst1(Plyr,D,D2),
                                                                                       Y = 4.
%5
b_move([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], Plyr, [A,B,C,D,E2,F,G,H,I,J,K,L,M,N,O,P],Y):-
                                                                                       length(E,X),
                                                                                       X<9,
                                                                                       addfirst1(Plyr,E,E2),
                                                                                       Y = 5.
%6
b_move([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], Plyr, [A,B,C,D,E,F2,G,H,I,J,K,L,M,N,O,P],Y):-
                                                                                       length(F,X),
                                                                                       X<9,
                                                                                       addfirst1(Plyr,F,F2),
                                                                                       Y = 6.
%7
b_move([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], Plyr, [A,B,C,D,E,F,G2,H,I,J,K,L,M,N,O,P],Y):-
                                                                                       length(G,X),
                                                                                       X<9,
                                                                                       addfirst1(Plyr,G,G2),
                                                                                       Y = 7.
%8
b_move([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], Plyr, [A,B,C,D,E,F,G,H2,I,J,K,L,M,N,O,P],Y):-
                                                                                       length(H,X),
                                                                                       X<9,
                                                                                       addfirst1(Plyr,H,H2),
                                                                                       Y = 8.
%9
b_move([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], Plyr, [A,B,C,D,E,F,G,H,I2,J,K,L,M,N,O,P],Y):-
                                                                                       length(I,X),
                                                                                       X<9,
                                                                                       addfirst1(Plyr,I,I2),
                                                                                       Y = 9.
%10
b_move([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], Plyr, [A,B,C,D,E,F,G,H,I,J2,K,L,M,N,O,P],Y):-
                                                                                       length(J,X),
                                                                                       X<9,
                                                                                       addfirst1(Plyr,J,J2),
                                                                                       Y = 10.
%11
b_move([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], Plyr, [A,B,C,D,E,F,G,H,I,J,K2,L,M,N,O,P],Y):-
                                                                                       length(K,X),
                                                                                       X<9,
                                                                                       addfirst1(Plyr,K,K2),
                                                                                       Y = 11.
%12
b_move([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], Plyr, [A,B,C,D,E,F,G,H,I,J,K,L2,M,N,O,P],Y):-
                                                                                       length(L,X),
                                                                                       X<9,
                                                                                       addfirst1(Plyr,L,L2),
                                                                                       Y = 12.
%13
b_move([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], Plyr, [A,B,C,D,E,F,G,H,I,J,K,L,M2,N,O,P],Y):-
                                                                                       length(M,X),
                                                                                       X<9,
                                                                                       addfirst1(Plyr,M,M2),
                                                                                       Y = 13.
%14
b_move([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], Plyr, [A,B,C,D,E,F,G,H,I,J,K,L,M,N2,O,P],Y):-
                                                                                       length(N,X),
                                                                                       X<9,
                                                                                       addfirst1(Plyr,N,N2),
                                                                                       Y = 14.
%15
b_move([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], Plyr, [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O2,P],Y):-
                                                                                       length(O,X),
                                                                                       X<9,
                                                                                       addfirst1(Plyr,O,O2),
                                                                                       Y = 15.
%16
b_move([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], Plyr, [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P2],Y):-
                                                                                       length(P,X),
                                                                                       X<9,
                                                                                       addfirst1(Plyr,P,P2),
                                                                                       Y = 16.

% ---- Player moves -----------------------------------
%1
w_move([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], 1, [A2,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]):-
                                                                                      length(A,X),
                                                                                      X<9,
                                                                                      addfirst1(w,A,A2).
%2
w_move([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], 2, [A,B2,C,D,E,F,G,H,I,J,K,L,M,N,O,P]):-
                                                                                       length(B,X),
                                                                                       X<9,
                                                                                       addfirst1(w,B,B2).
%3
w_move([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], 3, [A,B,C2,D,E,F,G,H,I,J,K,L,M,N,O,P]):-
                                                                                       length(C,X),
                                                                                       X<9,
                                                                                       addfirst1(w,C,C2).
%4
w_move([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], 4, [A,B,C,D2,E,F,G,H,I,J,K,L,M,N,O,P]):-
                                                                                       length(D,X),
                                                                                       X<9,
                                                                                       addfirst1(w,D,D2).
%5
w_move([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], 5, [A,B,C,D,E2,F,G,H,I,J,K,L,M,N,O,P]):-
                                                                                       length(E,X),
                                                                                       X<9,
                                                                                       addfirst1(w,E,E2).
%6
w_move([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], 6, [A,B,C,D,E,F2,G,H,I,J,K,L,M,N,O,P]):-
                                                                                       length(F,X),
                                                                                       X<9,
                                                                                       addfirst1(w,F,F2).
%7
w_move([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], 7, [A,B,C,D,E,F,G2,H,I,J,K,L,M,N,O,P]):-
                                                                                       length(G,X),
                                                                                       X<9,
                                                                                       addfirst1(w,G,G2).
%8
w_move([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], 8, [A,B,C,D,E,F,G,H2,I,J,K,L,M,N,O,P]):-
                                                                                       length(H,X),
                                                                                       X<9,
                                                                                       addfirst1(w,H,H2).
%9
w_move([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], 9, [A,B,C,D,E,F,G,H,I2,J,K,L,M,N,O,P]):-
                                                                                       length(I,X),
                                                                                       X<9,
                                                                                       addfirst1(w,I,I2).
%10
w_move([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], 10, [A,B,C,D,E,F,G,H,I,J2,K,L,M,N,O,P]):-
                                                                                       length(J,X),
                                                                                       X<9,
                                                                                       addfirst1(w,J,J2).
%11
w_move([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], 11, [A,B,C,D,E,F,G,H,I,J,K2,L,M,N,O,P]):-
                                                                                       length(K,X),
                                                                                       X<9,
                                                                                       addfirst1(w,K,K2).
%12
w_move([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], 12, [A,B,C,D,E,F,G,H,I,J,K,L2,M,N,O,P]):-
                                                                                       length(L,X),
                                                                                       X<9,
                                                                                       addfirst1(w,L,L2).
%13
w_move([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], 13, [A,B,C,D,E,F,G,H,I,J,K,L,M2,N,O,P]):-
                                                                                       length(M,X),
                                                                                       X<9,
                                                                                       addfirst1(w,M,M2).
%14
w_move([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], 14, [A,B,C,D,E,F,G,H,I,J,K,L,M,N2,O,P]):-
                                                                                       length(N,X),
                                                                                       X<9,
                                                                                       addfirst1(w,N,N2).
%15
w_move([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], 15, [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O2,P]):-
                                                                                       length(O,X),
                                                                                       X<9,
                                                                                       addfirst1(w,O,O2).
%16
w_move([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], 16, [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P2]):-
                                                                                       length(P,X),
                                                                                       X<9,
                                                                                       addfirst1(w,P,P2).


%w_move(Brd, _, Brd) :- write('Illegal move.'), nl.


%---------- W_move check --------------------------------

w_moveCheck(Brd,N,NewBrd):-
                   w_move(Brd,N,NewBrd).

w_moveCheck(Brd,N,NewBrd):-
                    not(w_move(Brd,N,NewBrd)),
                    writeln('this place is full !!'),
                    writeln('please choose another number'),
                    read(N2),
                    w_moveCheck(Brd,N2,NewBrd).

%---------- el(I,L,X) -----------------------------------

el(I,L,X):-
	el2(I,1,L,X).

el2(I,J,[H|_],X):-
	I==J,
	X=H,! .

el2(I,J,[_|T],X):-
	I\=J,
	J2 is J+1,
	el2(I,J2,T,X).

%------- w check ---------------------------------------

check([[A|_],[B|_],[C|_],[D|_],[E|_],[F|_],[G|_],[H|_],[I|_],[J|_],[K|_],[L|_],[M|_],[N|_],[O|_],[P|_]],_,W1orB1,X):-

        W1orB1 == 20 ;
        (A\=X,B\=X,C\=X,D\=X,E\=X,F\=X,G\=X,H\=X,I\=X,J\=X,K\=X,L\=X,M\=X,N\=X,O\=X,P\=X) .

%1
check(Brd,N,W1orB1,X):-          % X = w or b
                 W1orB1 > -1,
                 N == 1,
                 N2 is N+1,
                 N3 is N+4,
                 el(N2,Brd,[H|_]),
                 el(N3,Brd,[H2|_]),
                 (H==X ; H2==X).
%2 or 3
check(Brd,N,W1orB1,X):-
                 W1orB1 > -1,
                (N == 2 ; N==3),
                 N2 is N+1,
                 N3 is N-1,
                 N4 is N+4,
                 el(N2,Brd,[H|_]),
                 el(N3,Brd,[H2|_]),
                 el(N4,Brd,[H3|_]),
                 (H==X ; H2==X ; H3==X).

%4
check(Brd,N,W1orB1,X):-
                 W1orB1 > -1,
                 N == 4,
                 N2 is N-1,
                 N3 is N+4,
                 el(N2,Brd,[H|_]),
                 el(N3,Brd,[H2|_]),
                 (H==X ; H2==X).
%5 or 9
check(Brd,N,W1orB1,X):-
                 W1orB1 > -1,
                (N == 5 ; N==9),
                 N2 is N+1,
                 N3 is N-4,
                 N4 is N+4,
                 el(N2,Brd,[H|_]),
                 el(N3,Brd,[H2|_]),
                 el(N4,Brd,[H3|_]),
                 (H==X ; H2==X ; H3==X).

%6 or 7 or 10 or 11
check(Brd,N,W1orB1,X):-
                 W1orB1 > -1,
                (N == 6 ; N==7 ;N == 10 ; N==11  ),
                 N2 is N+1,
                 N3 is N-1,
                 N4 is N+4,
                 N5 is N-4,
                 el(N2,Brd,[H|_]),
                 el(N3,Brd,[H2|_]),
                 el(N4,Brd,[H3|_]),
                 el(N5,Brd,[H4|_]),
                 (H==X ; H2==X ; H3==X ; H4==X).

%8 or 12
check(Brd,N,W1orB1,X):-
                 W1orB1 > -1,
                (N == 8 ; N==12),
                 N2 is N-1,
                 N3 is N-4,
                 N4 is N+4,
                 el(N2,Brd,[H|_]),
                 el(N3,Brd,[H2|_]),
                 el(N4,Brd,[H3|_]),
                 (H==X ; H2==X ; H3==X).

%14 or 15
check(Brd,N,W1orB1,X):-
                 W1orB1 > -1,
                (N==14 ; N==15),
                 N2 is N+1,
                 N3 is N-4,
                 N4 is N-1,
                 el(N2,Brd,[H|_]),
                 el(N3,Brd,[H2|_]),
                 el(N4,Brd,[H3|_]),
                 (H==X ; H2==X ; H3==X).

%13
check(Brd,N,W1orB1,X):-
                 W1orB1 > -1,
                 N == 13,
                 N2 is N+1,
                 N3 is N-4,
                 el(N2,Brd,[H|_]),
                 el(N3,Brd,[H2|_]),
                 (H==X ; H2==X).
%16
check(Brd,N,W1orB1,X):-
                 W1orB1 > -1,
                 N == 16,
                 N2 is N-1,
                 N3 is N-4,
                 el(N2,Brd,[H|_]),
                 el(N3,Brd,[H2|_]),
                 (H==X ; H2==X).

%----------------------------------------------------
%check w neighbour
check2(Brd,N,W1,X,N3):-
                   check(Brd,N,W1,X),
                   N3 = N.

check2(Brd,N,W1,X,N3):-
                    not(check(Brd,N,W1,X)),
                    writeln('move is invalid'),
                    writeln('please choose a neighbour number'),
                    read(N2),
                    check2(Brd,N2,W1,X,N3).

%check b neighbour
check3(Brd,N,B1,X,_):-
                   check(Brd,N,B1,X).

% ----- go & howToPlay ----------------------------------

go :-
    howToPlay,
    start([[1],[2],[3],[4],[5],[6],[7],[8],[9],[10],[11],[12],[13],[14],[15],[16]],20,20).


howToPlay :-

  nl,write('You are "W" player, enter positions followed by a period.'),nl,nl,
  disp([[1],[2],[3],[4],[5],[6],[7],[8],[9],[10],[11],[12],[13],[14],[15],[16]],20,20).


%----- display ------------------------------------------

disp([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P],W1,B1) :-

        write('w : '),writeln(W1),
        write('b : '),writeln(B1),
        writeln(' '),
	write(A),write(' , '),write(B),write(' , '),write(C),write(' , '),write(D),nl,
        write(E),write(' , '),write(F),write(' , '),write(G),write(' , '),write(H),nl,
        write(I),write(' , '),write(J),write(' , '),write(K),write(' , '),write(L),nl,
        write(M),write(' , '),write(N),write(' , '),write(O),write(' , '),write(P),nl,nl.

% ---- start -------------------------------------------

start(Brd,_,_):-
          (win(Brd, w), write('You win!'));
          (win(Brd, b), write('AI win!')),!.



start(_,_,B1):-

	  B1 == 0,
	  writeln(''),
	  writeln('The game is equal !!! '),
	  writeln(''),!.


start(Brd,W1,B1):-
          writeln('choose one item :'),
	  writeln(' '),
          writeln('1.new move'),
          writeln('2.swap'),
	  writeln(' '),
          read(N),
          start(N,Brd,W1,B1).

%New Move
start(1,Brd,W1,B1):-

	      W1 \= 0,
	      B1 \=0,

	      writeln(' '),
	      writeln('choose a number'),
	      writeln(' '),

              read(N),
              check2(Brd,N,W1,w,N2),
              w_moveCheck(Brd, N2, NewBrd),
              W2 is W1-1,

	      writeln(' '),
	      writeln('player : white'),
	      writeln(' '),

	      disp(NewBrd,W2,B1),

              b_play(NewBrd, NewNewBrd,B1),
              B2 is B1-1,

	      writeln('player : black'),
	      writeln(' '),

	      disp(NewNewBrd,W2,B2),
              start(NewNewBrd,W2,B2).

% wrong  input
start(2,Brd,20,20):-
		      writeln(' '),
		      writeln('You can not swap as first move !!!'),
		      writeln(' '),
		      start(Brd,20,20).
%Swap
start(2,Brd,W1,B1):-
		       W1\= 20, % avale kar nabashad
		       B1\= 20, % avale kar nabashad

		       B1\=0,   % bazi tamam nashode bashad


	               writeln(' '),
	               writeln('Choose a number'),
	               writeln(' '),

	               read(N),			 % start place

		       writeln(' '),
		       writeln('Insert your swap list ([[N1,M1],...,[N3,M3]) :'),
	               writeln(' '),

		       read(N2),	         % list of transport

		       length(N2,X),
		       swap(X,N2,N,Brd,NewBrd),

	               writeln(' '),
	               writeln('player : white'),
	               writeln(' '),

		       disp(NewBrd,W1,B1),
		       b_play(NewBrd, NewNewBrd,B1),
                       B2 is B1-1,

	               writeln('player : black'),
	               writeln(' '),

	               disp(NewNewBrd,W1,B2),
		       start(NewNewBrd,W1,B2).

%---------------------------------------------------------

seprate(N,Brd,H1,H2,NewNewBrd):-

			   el(N,Brd,L),	           % src list
			   el(H1,Brd,L2),	   % dist list

			   seprate2(L,H2,1,L3,L4), % L3 = update of L , L4 = listi ke be L2 bayad add first shavad

			   addfirst(L4,L2,L5),     % L5 = update of L2

			   push(N,Brd,L3,NewBrd),
			   push(H1,NewBrd,L5,NewNewBrd),!.

seprate2([H|T],H2,J,L3,L4):-
			J==H2,
			L4=[H],
			L3=T,!.

seprate2([H|T],H2,J,L3,[H|T2]):-
		       J\=H2,
		       J2 is J+1,
		       seprate2(T,H2,J2,L3,T2),!.



%---------------------------------------------------------

swap(1,[[H1|[H2|_]]],N,Brd,NewBrd):-

	  in_same_direct(N,H1,0,0),

	  el(N,Brd,[H|_]),         %head of selected place should be "W"
	  H \= b,

          el(N,Brd,L),		   %CHECK SUM
	  length(L,LengthN1),
	  LengthN2 is LengthN1-1,
	  H2=<LengthN2,

	  el(H1,Brd,L1),
	  length(L1,LengthH1),
	  Sum2 is H2+LengthH1,
	  Sum2<10,!,

	  seprate(N,Brd,H1,H2,NewBrd).


swap(2,[[H1|[H2|_]],[H3|[H4|_]]],N,Brd,NewNewBrd):-

	  in_same_direct(N,H1,H3,0),

	  el(N,Brd,[H|_]),         %head of selected place should be "W"
	  H \= b,

	  el(N,Brd,L),		   %CHECK SUM
	  length(L,LengthN1),
	  LengthN2 is LengthN1-1,
	  Sum is H2+H4,
	  Sum=<LengthN2,

	  el(H1,Brd,L1),
	  length(L1,LengthH1),
	  Sum2 is H2+LengthH1,
	  Sum2<10,

	  seprate(N,Brd,H1,H2,NewBrd),

	  el(H3,Brd,L3),
	  length(L3,LengthH3),
	  Sum3 is H4+LengthH3,
	  Sum3<10,!,

	  seprate(N,NewBrd,H3,H4,NewNewBrd).

swap(3,[[H1|[H2|_]],[H3|[H4|_]],[H5|[H6|_]]],N,Brd,NewNewNewBrd):-

	  in_same_direct(N,H1,H3,H5),

	  el(N,Brd,[H|_]),         %head of selected place should be "W"
	  H \= b,

	  el(N,Brd,L),		   %CHECK SUM
	  length(L,LengthN1),
	  LengthN2 is LengthN1-1,
	  Sum1 is H2+H4+H6,
	  Sum1=<LengthN2,

	  el(H1,Brd,L1),
	  length(L1,LengthH1),
	  Sum2 is H2+LengthH1,
	  Sum2<10,

	  seprate(N,Brd,H1,H2,NewBrd),

	  el(H3,Brd,L3),
	  length(L3,LengthH3),
	  Sum3 is H4+LengthH3,
	  Sum3<10,

	  seprate(N,NewBrd,H3,H4,NewNewBrd),

	  el(H5,Brd,L5),
	  length(L5,LengthH5),
	  Sum5 is H6+LengthH5,
	  Sum5<10,

	  seprate(N,NewNewBrd,H5,H6,NewNewNewBrd),!.


swap(_,_,_,_,Brd):-

	  writeln(' '),
	  writeln('input is incorrect, please make it correct'),
	  writeln(' '),

	  writeln('Choose a number'),
	  writeln(' '),

          read(N),       % start place

	  writeln(' '),
	  writeln('Insert your swap list ([[N1,M1],...,[N3,M3]) :'),
	  writeln(' '),

	  read(N2),      % list of transport

	  length(N2,X),
	  swap(X,N2,N,Brd,_).


% ---------- AI win after 1 move --------------------------

b_play(Brd,NewBrd,B1) :-
                      b_move(Brd, b, NewBrd,X1),
                      check3(Brd,X1,B1,b,_),
                      el(X1,Brd,[Y|_]),
                      Y\=b,
                      win(NewBrd, b),!.


% ---mitoonim ba ye harekat joloye borde w ro begirim?--------------------
% [w,b,1],[w,b,2],[w,3],[4] ===> 3

b_play(Brd,NewBrd,B1) :-

		      can_w_win(Brd),
		      b_move(Brd, b, NewBrd,X),

                      check3(Brd,X,B1,b,_),
                      el(X,Brd,[Y|_]),
                      Y\=b,

		      el(X,Brd,Z),
                      ((rev(Z,Z2), el(2,Z2,Z3),Z3\=b);(length(Z,Lgth),Lgth==1)),

		      not(can_w_win(NewBrd)).

% ---mitoonim ba ye harekat joloye borde w ro begirim?--------------------
%[w,b,1],[w,b,2],[w,b,3],[4] ===> 1

b_play(Brd,NewBrd,B1) :-
		      can_w_win(Brd),
		      b_move(Brd, b, NewBrd,X),
                      check3(Brd,X,B1,b,_),
                      el(X,Brd,[Y|_]),
                      Y\=b,
                      not(can_w_win(NewBrd)).


%-- put b in neighbour of w ----------------------------------

b_play([[A|TA],[B|TB],[C|TC],[D|TD],[E|TE],[F|TF],[G|TG],[H|TH],[I|TI],[J|TJ],[K|TK],[L|TL],[M|TM],[N|TN],[O|TO],[P|TP]],NewBrd,_) :-

		    (A\=b,B\=b,C\=b,D\=b,E\=b,F\=b,G\=b,H\=b,I\=b,J\=b,K\=b,L\=b,M\=b,N\=b,O\=b,P\=b),
		    b_move([[A|TA],[B|TB],[C|TC],[D|TD],[E|TE],[F|TF],[G|TG],[H|TH],[I|TI],[J|TJ],[K|TK],[L|TL],[M|TM],[N|TN],[O|TO],[P|TP]], b, NewBrd,X),
		    check3(NewBrd,X,1,w,_).


% ------ check if AI win after 2 move ----------------------

b_play(Brd,NewBrd,B1) :-
                      b_move(Brd, b, NewBrd,X1),
                      check3(Brd,X1,B1,b,_),
                      el(X1,Brd,[Y|_]),
                      Y\=b,

		      b_move(NewBrd, b, NewNewBrd,X2),
                      check3(NewBrd,X2,B1,b,_),
                      el(X2,NewBrd,[Y2|_]),
                      Y2\=b,

		      win(NewNewBrd, b),!.

% ------ check if AI win after 3 move ----------------------

b_play(Brd,NewBrd,B1) :-
                      b_move(Brd, b, NewBrd,X1),
                      check3(Brd,X1,B1,b,_),
                      el(X1,Brd,[Y|_]),
                      Y\=b,
		      Y\=w,

		      b_move(NewBrd, b, NewNewBrd,X2),
                      check3(NewBrd,X2,B1,b,_),
                      el(X2,NewBrd,[Y2|_]),
                      Y2\=b,

		      b_move(NewNewBrd, b, NewNewNewBrd,X3),
                      check3(NewNewBrd,X3,B1,b,_),
                      el(X3,NewNewBrd,[Y3|_]),
                      Y3\=b,

		      win(NewNewNewBrd, b),!.



% -----------  put b some where --------------------

b_play(Brd,NewBrd,B1) :-
		      b_move(Brd, b, NewBrd,X),
                      check3(Brd,X,B1,b,_),
                      el(X,Brd,[Y|_]),
                      Y\=b,
                      Y\=w.


%-------------  put b some where -------------------

b_play(Brd,NewBrd,B1) :-
		      b_move(Brd, b, NewBrd,X),
                      check3(Brd,X,B1,b,_),
                      el(X,Brd,[Y|_]),
                      Y\=b.
%---------------------------------------------------------

can_w_win(Brd) :-
	              b_move(Brd, w, NewBrd,_),
                      win(NewBrd, w).



%---------------------------------------------------------



















