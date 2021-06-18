%author(AuthorId, AuthorName)
author(1, "Isaac Asimov").
author(2, "Frank Herbert").
author(3, "William Morris").
author(4, "J.R.R Tolkein").

%genre(GenreId, Genre)
genre(1, "Science").
genre(2, "Literature").
genre(3, "Science Fiction").
genre(4, "Fantasy").

%book(Name, AuthorId, GenreId, Length)
book("Inside The Atom", 1, 1, 500).
book("Asimov's Guide To Shakespeare", 1, 2, 400).
book("I, Robot", 1, 3, 450).
book("Example1", 1, 3, 750).
book("Dune", 2, 3, 550).
book("The Well at the World's End", 3, 4, 400).
book("The Hobbit", 4, 4, 250).
book("The Lord of the Rings", 4, 4, 1250).

%Signature:  authorOfGenre(GenreName, AuthorName)/2
%Purpose: {AuthorName} has written a book belonging to the genre named {GenreName}.
authorOfGenre(GenreName,AuthorName) :- author(N,AuthorName), book(_,N,GenreName,_).


%bookFound(BookName, Length) :- book(BookName,_,_,Length).

%Signature:  longestBook(AuthorId, BookName)/2
%Purpose: Longest book that an author with the ID {AuthorId} has written in titled {BookName}
longestBook(AuthorId,BookName) :- 
    book(BookName,AuthorId,_,Length),
    findall(bookFound(X,Y),book(X,AuthorId,_,Y),List),
    max_member(bookFound(BookName,Length),List).


%Signature:  versatileAuthor(AuthorName)/1
%Purpose:  author by the name {AuthorName} has written books in at least three different genres
versatileAuthor(AuthorName) :-
    author(Id,AuthorName),
    findall(G,book(_,Id,G,_),List),
    list_to_set(List,Set),
    length(Set,N),
    N \= 0, N \= 1, N \= 2.


