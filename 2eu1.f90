          !Método de Euler para la función y''+6y'+9y=0, que podemos resolver como y1=y, y2=y' , obteniendo 
          !y2'=-6y2-9y1, y1'=y2 con y1(0)=4, y2(0)=-4
          !tiempo 0.01
          real(kind=4):: y1(0:300),y2(0:300),ta,t,dt,h,x
          open(1,file='eu1.dat',status='replace')
          y1(0)=4.0
          y2(0)=-4.0
          ta=0.01
          dt=0.01
          write(1,*)'0.0', '             ',y1(0) ,y2(0) 
          do i=1,300
           j=i-1
           t=j*ta
           x=t+dt  
           y1(i)=y1(i-1)+y2(i-1)*dt
           y2(i)=y2(i-1)+(-6.*y2(i-1)-9.*y1(i-1))*dt
           !tiempo,solución de y,valor de y´, solución analítica
           print*, x,y1(i),y2(i), exp(-3.*x)*(8.*x+4.) 
           write(1,*)x,y1(i),y2(i), exp(-3.*x)*(8.*x+4.)
          enddo
          end
          
