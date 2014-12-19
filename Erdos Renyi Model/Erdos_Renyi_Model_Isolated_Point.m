function [ num_of_ip ] = Erdos_Renyi_Model_Isolated_Point ( N,p )

%Demo for Erdos Renyi Model on Isolated Point
%openopen 2014/Dec/06
%openopen114@gmail.com

%input-----
%set up 'N' for num of node
%set up 'p' for the probability of an edge being present or absent

%output----
%num_of_ip: num of isolated point

i_idx=[]; % sparse matrix i
j_idx=[]; % sparse matrix j
s_val=[]; % sparse matrix s=(i,j)value

num_of_ip=0;  %ip=isolated point 

for i=1:1:N
    for j=(i+1):1:N
        
        rand_num=rand()
           if rand_num<=p  %'connect'
               
               i_idx=[i_idx,i,j];
               j_idx=[j_idx,j,i];
               s_val=[s_val,1,1];
                 
           else           %'disconnect'
                   
               i_idx=[i_idx,i,j];
               j_idx=[j_idx,j,i];
               s_val=[s_val,0,0];
           end
    end
end


Edge_Sparse=sparse(i_idx,j_idx,s_val,N,N);  % sparse matrix
Edge_Sparse_Full=full(Edge_Sparse)


%check isolated point%

%select row with most 1 ie. less 0(ip) for check
[max_val,max_i]=max(sum(Edge_Sparse_Full,2));
if max_val==N; %all equal 1 ie. no ip
    num_of_ip=0;
else
    [i_max_zero,j_max_zero]=find(Edge_Sparse_Full(max_i,:)<1); %find 0 row
    
    for i=j_max_zero
        if sum(Edge_Sparse_Full(i,:))==0 %check all 0 or not
            num_of_ip=num_of_ip+1;
        end
    end
end


%p=c*log(N)/N--threshold
c=p/(log(N)/N);
if c>1
    disp('c > 1,(Isolated Point = 0)')
else
    disp('c < 1, (Isolated Point > 0)')
end

%disp how many bad pair
sprintf('num of Isolated Point = %d',num_of_ip)


end

