          !Programa que utiliza Runge-kutta          
          !Método de modificación de Euler para  y''+6y'+9y=0, que podemos resolver como y1=y, y2=y' , obteniendo 
          !y2'=-6y2-9y1, y1'=y2 con y1(0)=4, y2(0)=-4
          !tiempo 0.001
          real(kind=4):: y1(0:3000),y2(0:3000),ta,h,x,f,t,g
          open(1,file='rk3.dat',status='replace')
          y1(0)=4.0
          y2(0)=-4.0
          ta=0.001
          h=0.001
          write(*,*)'0.0', '             ',y1(0) ,y2(0)
          do i=1,3000
           j=i-1
           t=j*ta
           x=t+h  
           y1(i)=y1(i-1)+(h/2.)*((f(t,y2(i-1)))+f(t+h,y2(i-1))+h*f(t,y2(i-1)))
           y2(i)=y2(i-1)+(h/2.)*((g(t,y1(i-1),y2(i-1)))+g(t+h,y1(i-1),y2(i-1))+h*g(t,y1(i-1),y2(i-1)))
           !tiempo,solución de y,valor de y´, solución analítica
           print*, x,y1(i),y2(i), exp(-3.*x)*(8.*x+4.) 
           write(1,*)x,y1(i),y2(i), exp(-3.*x)*(8.*x+4.)
          enddo
          end
          function f(t,y2)
          real y2,t
           f=y2
          end
          function g(t,y1,y2)
          real y1,y2,t
           g=-9.*y1-6.*y2
          end

