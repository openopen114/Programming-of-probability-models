function random_graph_Main
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here

    tic
    cyc=[]
    cyc_temp=[]
    e=[]
    for n=5:5:100 %set up N---
        
         d=random_graph_E_connect(n);
         e=[e,d];
         
         
        %Simulation 1000times
        for i=1:1:1000  %set up # of trial---
            a=random_graph_conncet(n);
            cyc_temp=[cyc_temp,a];
            size_cyc_temp=size(cyc_temp,2);
           fprintf('times = %d_%d......\n ',n,size_cyc_temp)%disp running which times
           
                 if i==1000,  %set up # of trial----
                    cyc=[cyc mean(cyc_temp)];
                    cyc_temp=[];
                 end
        end
    end
    
    %set up file type to double
    cycle=double(vpa(cyc,2));
    Exp_connect=double(vpa(e,2));
    result5_100=double(vpa([cycle;Exp_connect],2));%----
    
    time5_100=toc%----
    
    
    
    %plot
    result=result5_100%----
    x=5:5:500;%set up N---
    y_cyc=result(1,:);
    y_Ec=result(2,:);
    plot(x,y_cyc,'b-^',x,y_Ec,'g--*');
    xlabel('number of node');
    ylabel('number of component');
    title('Simulation for random graph connect');
    legend('simulation of component','expecation of component');
    axis([0,500,0,6]);%----
    
    
    %test
    x=result(1,:);
    y=result(2,:);
    
    %F test (variances of the two samples are equal)
    [h_F,p_F]=vartest2(x,y)
    
    %Paired T test (mean of the two samples are equal)
    [h_T,p_T]=ttest2(x,y)
    

end

