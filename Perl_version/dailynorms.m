# first, the leap year case:

# one row per day, column per month
mmask = zeros(366,12);
mmask(1:31,1) = 1;
mmask(32:60,2) = 1;
mmask(61:91,3) = 1;
mmask(92:121,4) = 1;
mmask(122:152,5) = 1;
mmask(153:182,6) = 1;
mmask(183:213,7) = 1;
mmask(214:244,8) = 1;
mmask(245:274,9) = 1;
mmask(275:305,10) = 1;
mmask(306:335,11) = 1;
mmask(336:366,12) = 1;

[mx, month] = max(mmask');
month = month';
csm = cumsum(mmask).*mmask;
day = max(csm')';

I=eye(366);

# smoothing matrix -- your choice, but row and column sums must be 1
# note that you need to put it in TWICE -- this 366x366 one and a
# normal-year one below.
S= 0.5*I + 0.25*([zeros(366,1) I(:,1:365)] + [I(:,2:366) zeros(366,1)]);
S(1,366)=0.25;
S(366,1)=0.25;

# block diagonal matrix which takes monthly averages
M=zeros(366,366);
M(1:31,1:31)=1/31;
M(32:60,32:60)=1/29;
M(61:91,61:91)=1/31;
M(92:121,92:121)=1/30;
M(122:152,122:152)=1/31;
M(153:182,153:182)=1/30;
M(183:213,183:213)=1/31;
M(214:244,214:244)=1/31;
M(245:274,245:274)=1/30;
M(275:305,275:305)=1/31;
M(306:335,306:335)=1/30;
M(336:366,336:366)=1/31;

# Finally the heart of it
A=I - (I-M)*S;
Wleap = inv(A)*mmask;


# now do the non-leap case
mmask=[mmask(1:59,:);mmask(61:366,:)];

I=eye(365);

# Smoothing matrix again -- this one is 365x365
S= 0.5*I + 0.25*([zeros(365,1) I(:,1:364)] + [I(:,2:365) zeros(365,1)]);
S(1,365)=0.25;
S(365,1)=0.25;

M=zeros(365,365);
# perl code to generate this repetitive stuff:
#@d=(31,28,31,30,31,30,31,31,30,31,30,31);
#$s=0;for $d (@d) { $t=$s+1; $s+=$d; print "M($t:$s,$t:$s)=1/$d;\n"; }
M(1:31,1:31)=1/31;                                                              
M(32:59,32:59)=1/28;                                                            
M(60:90,60:90)=1/31;                                                            
M(91:120,91:120)=1/30;                                                          
M(121:151,121:151)=1/31;                                                        
M(152:181,152:181)=1/30;                                                        
M(182:212,182:212)=1/31;                                                        
M(213:243,213:243)=1/31;                                                        
M(244:273,244:273)=1/30;                                                        
M(274:304,274:304)=1/31;                                                        
M(305:334,305:334)=1/30;                                                        
M(335:365,335:365)=1/31;                                                        
 
A=I - (I-M)*S;
Wnorm = inv(A)*mmask;

Wpre = 0.75*Wnorm(1:59,:)+0.25*Wleap(1:59,:);
Wpost = 0.75*Wnorm(60:365,:)+0.25*Wleap(61:366,:);
W29 = Wleap(60,:);
W = [Wpre; W29; Wpost];

# prepend the month and day columns for dailynorms.pl
Wplus = [month day W];

# now save them
save("-ascii", "/tmp/W", "W")
save("-ascii", "/tmp/Wplus", "Wplus")

