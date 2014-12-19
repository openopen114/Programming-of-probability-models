function [ k ] = Best_Prize_Problem(n)
%Demo for "best prize problem" Page.126
%openopen 19-Dec-2014
%openopen114@gmail.com

% n : number of prizes
% k :reject the first k prizes


plist = [];
for k = 1:1:n-1,
    i=sym('i'); 
    p = (k/n) * symsum(1/(i-1), k+1, n);
    plist = [plist, p];        
end
    [Max_value, Max_i]=max(plist);  
    k = Max_i;

    
    
    
end