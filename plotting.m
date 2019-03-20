%% Age of Population
ptitle = 'Age Distribution of Population';
m1 = csvread('tab1.csv');
h=histogram(m1,'BinEdges',20:5:80);
hold on; D2 = h.BinEdges; D=D2(2:end); F = h.Values; p=polyfit(D,F,1); f=polyval(p,D2); plot(D2,f,'--r','LineWidth',2) 
xlabel('Age','FontSize',16);ylabel('Number of People','FontSize',16);

%% Age of AF Patients
ptitle = 'Age of AF Patients';
m1 = csvread('tab2.csv');
histogram(m1,'BinEdges',20:5:80)
xlabel('Age','FontSize',16);ylabel('Count','FontSize',16)

%% Age of AF Patients Pie chart
colormap(hsv)
ptitle = 'Age of AF Patients';
m1 = [3   4   4   9  19  25  23  34  47  51  53 172];
labels = {'20','25','30','35','40','45','50','55','60','65','70','75+'};
pie(m1,labels)

%% Prevalence of AF in Population by Age Group
ptitle = 'Prevalence of AF in Population by Age Group';
m1 = [0.1583949  0.2779708  0.2749141  0.5821475  1.2467192  1.6666667  1.5625000  2.3208191  3.0718954  4.4270833  5.4192229 10.2564103];
x = 20:5:75;
colormap(lines);bar(x,m1); xlim([15 80])
ylabel('% Prevalence','FontSize',16); xlabel('Age Group','FontSize',16)
ylim([0 11])

%% Age Distribution of Male and Female AF patients
ptitle = 'Age Distribution of Male and Female AF patients';
m1 = csvread('tab3.csv');m2 = csvread('tab4.csv');
subplot(1,2,1);histogram(m1,'BinEdges',20:5:80, 'Normalization', 'probability');xlabel('Age','FontSize',12);ylabel('Probability','FontSize',12);ylim([0 0.45])
title('Male AF Age Distribution','FontSize',12)
subplot(1,2,2);histogram(m2,'BinEdges',20:5:80,'Normalization', 'probability');title('Female AF Age Distribution','FontSize',12)
xlabel('Age','FontSize',12);ylabel('Probability','FontSize',12);

%% Age Distribution of Obese AF and non AF patients
ptitle = 'Age Distribution of Obese AF and non AF patients PREVALENCES';
m1 = csvread('tab5.csv');m2 = csvread('tab6.csv');
h=histogram(m1,'BinEdges',20:5:80, 'Normalization', 'probability');
u = h.Values;
uu=h.BinEdges;
close all
subplot(1,2,1);h=bar(20:5:75,0.3539587*u*100);xlabel('Age','FontSize',12);ylabel('% Prevalence in Population','FontSize',12);ylim([0 0.27*0.3595735*100]);xlim([15 80])
l=lines;
ylim([0 12])
set(h, 'FaceColor',l(1,:));
hold on; subplot(1,2,1); D2 = uu; D=D2(2:end); F = u*0.3539587*100; p=polyfit(D,F,1); f=polyval(p,D2); plot(D2,f,'--r','LineWidth',2) 
title('Obesity Prevalence in Total Population','FontSize',12)

subplot(1,2,2);m1 = csvread('tab14.csv');
colormap(lines)
b=bar(20:5:75,m1*100);
l=lines;set(b,'FaceColor',l(2,:));
xlim([15 80])
xlabel('Age','FontSize',15);ylabel('% Prevalence in Obese Population','FontSize',12);
title('AF Prevalence in Obese Population','FontSize',12)

%% Disease distribution of AF and non-AF patients
%split this into multiple graphs

ptitle = 'Disease distribution of AF and control over 60';
m1 = csvread('tab7.csv');
h= histogram(m1,'Normalization', 'probability');
m1=h.Values;
m2 = csvread('tab8.csv');
h= histogram(m2,'Normalization', 'probability');
m2=h.Values;

%m1 = [0.1126 0.3288 0.3131 0.1847 0.04505 0.01577];
%m2 = [0.4275 0.3264 0.1641 0.06592 0.01222 0.00384];
m3 = [m2' m1'];
b=bar(0:5,m3);
c=colormap(lines);
b(1).FaceColor = c(1,:);
b(2).FaceColor = c(2,:);
xlabel('Number of Concurrent Diseases','FontSize',16);ylabel('Probability','FontSize',16);ylim([0 0.45]);xlim([-0.5 5.5])
legend('Non-AF','AF')
title('Concurrent Diseases for AF and Non-AF over 60','FontSize',22)

%% Bar Plot of Relative Risk Confidence Intervals
ptitle = 'Bar Plot of Relative Risk Confidence Intervals';
Names = {'White Ethnicity';'Thyroid Condition';'Obesity';'Congestive Heart';'High BP';'Over 65'};
L = [1.0440732 1.0970063 1.1428308 1.4286815 1.5823469 2.1816352];
U = [1.951581 2.3340881 1.8793914 4.0309083 3.5428491 3.9339257];
av = mean([L;U])';
b=bar(av);
colormap(lines)
set(gca,'xticklabel',Names)
hold on
n=10;d=0.1;
for i=1:length(L)
    plot(linspace(i,i,n),linspace(L(i),U(i),n), 'r')
    plot(linspace(i-d,i+d,n),linspace(L(i),L(i),n),'r')
    plot(linspace(i-d,i+d,n),linspace(U(i),U(i),n),'r')
end
xlim([0.5 6.5]); 
plot(linspace(0.5,6.5,50),linspace(1,1,50),'k--')
xlabel('Risk Factor','FontSize',16);ylabel('Relative Risk','FontSize',16);

hold off

%% Age Distribution of High BP AF and non AF patients
ptitle = 'Age Distribution of High BP AF and non AF patients PREVALENCES';
m1 = csvread('tab9.csv');m2 = csvread('tab10.csv');
h=histogram(m1,'BinEdges',20:5:80, 'Normalization', 'probability');
u = h.Values;
close all
subplot(1,2,1);h=bar(20:5:75,0.3595735*u*100);xlabel('Age','FontSize',12);ylabel('% Prevalence in Population','FontSize',12);ylim([0 0.42*0.3595735*100]);xlim([15 80])
l=lines;
ylim([0 13])
set(h, 'FaceColor',l(1,:));
%hold on; subplot(1,2,1); D2 = h.BinEdges; D=D2(2:end); F = h.Values; p=polyfit(D,F,1); f=polyval(p,D2); plot(D2,f,'--r','LineWidth',2) 
title('High BP Prevalence in Total Population','FontSize',12)

subplot(1,2,2);m1 = csvread('tab17.csv');
colormap(lines)
b=bar(20:5:75,m1*100);
l=lines;set(b,'FaceColor',l(2,:));
xlim([15 80]);ylim([0 13])
xlabel('Age','FontSize',12);ylabel('% Prevalence in High BP Population','FontSize',12);
title('AF Prevalence in High BP Patients','FontSize',12)


%% Prevalence of AF by age
ptitle = 'Prevalence of AF by Age';
m1 = csvread('tab11.csv');
labels = {'20','25','30','35','40','45','50','55','60','65','70','75+'};
colormap(lines)
bar(20:5:75,m1*100)
xlim([15 80])
xlabel('Age','FontSize',15);ylabel('% Prevalence in Population','FontSize',15);
title('Prevalence of AF','FontSize',22)

%% Prevalence of AF by age in Obese people
ptitle = 'Obese Prevalence of AF by Age';
m1 = csvread('tab14.csv');
colormap(lines)
bar(20:5:75,m1*100)
xlim([15 80])
xlabel('Age','FontSize',15);ylabel('% Prevalence in Obese Population','FontSize',15);

%% Disease distribution of AF and non-AF patients (my risk factors)

ptitle = 'Disease distribution of AF and Control SPLIT';
m1 = csvread('tab15.csv');
h= histogram(m1,'Normalization', 'probability');
m1=h.Values;
m2 = csvread('tab16.csv');
h= histogram(m2,'Normalization', 'probability');
m2=h.Values;

close all
c=colormap(lines);
subplot(1,2,1)
b=bar(0:5,m2);
set(b,'FaceColor',c(1,:))
xlabel({'Number of Concurrent', 'Risk Factors'},'FontSize',12);ylabel('Probability','FontSize',12);ylim([0 0.45]);xlim([-1 6])
title({'Number of Concurrent', 'Risk Factors for Non-AF'},'FontSize',12)

subplot(1,2,2)
b=bar(0:5,m1);
set(b,'FaceColor',c(2,:))
xlabel({'Number of Concurrent', 'Risk Factors'},'FontSize',12);ylabel('Probability','FontSize',12);ylim([0 0.45]);xlim([-1 6])
title({'Number of Concurrent', 'Risk Factors for AF'},'FontSize',12)


%% Prevalences of Male and Female

ptitle = 'Male Female Prevalence';
m1 = csvread('tab20.csv');
m2 = csvread('tab21.csv');
subplot(1,2,1);
b=bar(20:5:75,m1*100); xlim([15 80]); ylim([0 12])
l=lines;
set(b,'FaceColor',l(1,:));
set(gca,'FontSize',8)
xlabel('Age','FontSize',12);ylabel('% Prevalence in Male Population','FontSize',12); title('Prevalence of AF in Males','FontSize',12); 

subplot(1,2,2);
b=bar(20:5:75,m2*100); xlim([15 80]); 
l=lines;
set(b,'FaceColor',l(1,:));
set(gca,'FontSize',8)
xlabel('Age','FontSize',12);ylabel('% Prevalence in Female Population','FontSize',12); title('Prevalence of AF in Females','FontSize',12); 

%% Disease distribution of AF and non-AF patients (my risk factors)

ptitle = 'Disease distribution of AF and Control SPLIT';
m1 = csvread('tab15.csv');
h= histogram(m1,'Normalization', 'probability');
m1=h.Values;
m2 = csvread('tab16.csv');
h= histogram(m2,'Normalization', 'probability');
m2=h.Values;

close all
c=colormap(lines);

b=bar(0:5,[m2' m1']);
set(b,'FaceColor',c(1,:))
xlabel({'Number of Concurrent', 'Risk Factors'},'FontSize',16);ylabel('Probability','FontSize',16);ylim([0 0.45]);xlim([-1 6])

c=colormap(lines);
b(1).FaceColor = c(1,:);
b(2).FaceColor = c(2,:);
legend('Non-AF','AF')



%% Disease distribution of AF and non-AF patients above 65

ptitle = 'Disease distribution of AF and Control 65';
m1 = csvread('tab31.csv');
h= histogram(m1,'Normalization', 'probability');
m1=h.Values;
m2 = csvread('tab32.csv');
h= histogram(m2,'Normalization', 'probability');
m2=h.Values;

close all
c=colormap(lines);

b=bar(0:5,[m2' m1']);
set(b,'FaceColor',c(1,:))
xlabel({'Number of Concurrent', 'Risk Factors'},'FontSize',16);ylabel('Probability','FontSize',16);ylim([0 0.45]);xlim([-1 6])

c=colormap(lines);
b(1).FaceColor = c(1,:);
b(2).FaceColor = c(2,:);
legend('Non-AF','AF')



%% MF
ptitle = 'male';
m1 = csvread('tab3.csv');m2 = csvread('tab4.csv');
histogram(m1,'BinEdges',20:5:80, 'Normalization', 'probability');xlabel('Age','FontSize',16);ylabel('Probability','FontSize',16);ylim([0 0.45])
ptitle = 'female';
histogram(m2,'BinEdges',20:5:80,'Normalization', 'probability');
xlabel('Age','FontSize',16);ylabel('Probability','FontSize',16);

%% Age Distribution of Obese AF and non AF patients
ptitle = 'Obese 2';
m1 = csvread('tab5.csv');m2 = csvread('tab6.csv');
h=histogram(m1,'BinEdges',20:5:80, 'Normalization', 'probability');
u = h.Values;
uu=h.BinEdges;
close all
subplot(1,2,1);

m1 = csvread('tab11.csv');
labels = {'20','25','30','35','40','45','50','55','60','65','70','75+'};
colormap(lines)
bar(20:5:75,m1*100)
xlim([15 80])
set(gca,'FontSize',7)
xlabel('Age','FontSize',15);ylabel('% Prevalence in Population','FontSize',12);
title({'AF Prevalence in','Total Population'},'FontSize',12)


subplot(1,2,2);m1 = csvread('tab14.csv');
colormap(lines)
b=bar(20:5:75,m1*100);
l=lines;set(b,'FaceColor',l(2,:));
xlim([15 80])
set(gca,'FontSize',7)
xlabel('Age','FontSize',15);ylabel('% Prevalence in Obese Population','FontSize',12);
title({'AF Prevalence in ','Obese Population'},'FontSize',12)

%% Age Distribution of High BP AF and non AF patients
ptitle = 'BP 2';
m1 = csvread('tab9.csv');m2 = csvread('tab10.csv');
h=histogram(m1,'BinEdges',20:5:80, 'Normalization', 'probability');
u = h.Values;
close all
subplot(1,2,1);

m1 = csvread('tab11.csv');
labels = {'20','25','30','35','40','45','50','55','60','65','70','75+'};
colormap(lines)
bar(20:5:75,m1*100)
xlim([15 80])
set(gca,'FontSize',7)
xlabel('Age','FontSize',15);ylabel('% Prevalence in Population','FontSize',12);
title({'AF Prevalence in','Total Population'},'FontSize',12)
ylim([0 13])
subplot(1,2,2);m1 = csvread('tab17.csv');
colormap(lines)
b=bar(20:5:75,m1*100);
l=lines;set(b,'FaceColor',l(1,:));
xlim([15 80]);ylim([0 13])
xlabel('Age','FontSize',16);ylabel('% Prevalence in High BP Population','FontSize',16);

%% Obese 1
ptitle='Obese1';
m1 = csvread('tab5.csv');m2 = csvread('tab6.csv');
h=histogram(m1,'BinEdges',20:5:80, 'Normalization', 'probability');
u = h.Values;
uu=h.BinEdges;
close all
h=bar(20:5:75,0.3539587*u*100);xlabel('Age','FontSize',16);ylabel('% Prevalence in Population','FontSize',16);ylim([0 6]);xlim([15 80])
l=lines;
ylim([0 12])
set(h, 'FaceColor',l(1,:));
hold on;  D2 = uu; D=D2(2:end); F = u*0.3539587*100; p=polyfit(D,F,1); f=polyval(p,D2); plot(D2,f,'--r','LineWidth',2) ;ylim([0 5])
%title('Obesity Prevalence in Total Population','FontSize',12)

%% BP 1
ptitle='BP1';

m1 = csvread('tab9.csv');m2 = csvread('tab10.csv');
h=histogram(m1,'BinEdges',20:5:80, 'Normalization', 'probability');
u = h.Values;
close all
h=bar(20:5:75,0.3595735*u*100);xlabel('Age','FontSize',16);ylabel('% Prevalence in Population','FontSize',16);xlim([15 80])
l=lines;
ylim([0 8])
set(h, 'FaceColor',l(1,:));
%hold on; subplot(1,2,1); D2 = h.BinEdges; D=D2(2:end); F = h.Values; p=polyfit(D,F,1); f=polyval(p,D2); plot(D2,f,'--r','LineWidth',2) 
