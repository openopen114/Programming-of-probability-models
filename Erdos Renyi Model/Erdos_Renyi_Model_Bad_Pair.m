function [ num_of_bp ] = Erdos_Renyi_Model_Bad_Pair ( N,p )

%Demo for Erdos Renyi Model on Bad Pair
%openopen 2014/Dec/06
%openopen114@gmail.com

%input-----
%set up 'N' for num of node
%set up 'p' for the probability of an edge being present or absent

%output----
%num_of_bp: num of bad pair


i_idx=[]; % sparse matrix i
j_idx=[]; % sparse matrix j
s_val=[]; % sparse matrix s=(i,j)value

num_of_bp=0;  %bp=bad pair
num_of_gp=0;  %gp=good pair

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


%check bad pair

%count how many GOOD pair
for i=1:1:N
    for j=(i+1):1:N
        
        %good pair on (i,j)=1 ie connect
        if Edge_Sparse_Full(i,j)==1  
           num_of_gp=num_of_gp+1;
        end
        
        %good pair on (i,j)=0 and row i and row j with the same position
        %for connect ie.1+1=2
        if Edge_Sparse_Full(i,j)==0 && sum(sum(Edge_Sparse_Full([i,j],:),1)==2)>0
           num_of_gp=num_of_gp+1;
        end
    end
end


all_pair=N*(N-1)/2;%num of all pair
num_of_bp=all_pair-num_of_gp; % num of bad pair=all - good pair

%p=c*sqrt(2)*sqrt(log(N))/sqrt(N)--threshold
c=p/(sqrt(2)*sqrt(log(N))/sqrt(N));
if c>1
    disp('c > 1,diameter <2 (num_of_bp=0)')
else
    disp('c < 1, diameter >2 (bad pair)')
end

%disp how many bad pair
sprintf('num of bad pair = %d',num_of_bp)

end

