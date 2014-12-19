function [ cycle ] = random_graph_conncet( N )

%Demo for "number of random graph connected components" Page.141
%openopen 19-Dec-2014
%openopen114@gmail.com

% N : is  number of node
% cycle : number of cycles


N1=[1:N]; % all point
N2=[1:N]; % not used point

connect_seq=[]; % final connect seq sep by "-7"
ori_seq=[]; % seq for check form a cycle or not 
N_value=[]; % point number selectd from N1 or N2(head point)
cycle=0;

for i=1:1:N^2,
     
    
    %random point
    if isempty(N_value),  %select for start from N2
       index_N2=randi([1,length(N2)],1,1);% index for N2
        N_value=N2(1,index_N2);
    else
        index_N1=randi([1,N],1,1);% select for seq form N1
        N_value=N1(1,index_N1);
    end

    %random point seq
    ori_seq=[ori_seq,N_value];
    
    
    % unique & hist for check form a cycle
    u_ori_seq=unique(ori_seq);  %unique
    h_ori_seq=hist(ori_seq,u_ori_seq);  %hist (data,levels)
    
    % N_value appeared on the connect_seq ie. connect to the supernode
    if sum(connect_seq==N_value)>0  % N_value appeared on the connect_seq connect to the supernode
       connect_seq=[connect_seq,ori_seq,-7]; 
       
        %det used point from N2
        for t=1:1:length(u_ori_seq)
            a=u_ori_seq(t);
            N2(N2==a)=[];
        end
        ori_seq=[];
        N_value=[];
         
     end
    
    
    %form a NEW cycle itself
     
     if sum(h_ori_seq==2) > 0, % 
        cycle=cycle+1;
        connect_seq=[connect_seq,ori_seq,-7];
        
        %det apper
        for t=1:1:length(u_ori_seq)
            a=u_ori_seq(t);
            N2(N2==a)=[];
        end
        ori_seq=[];
        N_value=[];
     end
     
     
     
        
        
     % when N2 is empty,stop the for loop 
      if isempty(N2),
         break;
      end
         
         
end
     




end








