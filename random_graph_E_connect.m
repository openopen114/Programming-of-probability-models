function [ EC ] = random_graph_E_connect( n )

%compute the  "the expected number of connected components" Page.148
%openopen 19-Dec-2014
%openopen114@gmail.com

% n : number of node
% EC : E[ number of cycles ]


syms k ;
EC=symsum([sym([num2str(n),'!'])*sym('(k-1)!')] ...,
        /[sym([num2str(n),'^k'])*sym('k!')*sym(['(',num2str(n),'-k)!'])],k,1,n) ;


end