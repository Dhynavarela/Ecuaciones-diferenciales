          !Programa que utiliza Runge-kutta          
          !Método de modificación de Euler  para la función y'=t*exp(3t)-2y, de 0 a 1 al tiempo 0.01
          real(kind=4):: y(0:100),ta,h,x,f
          open(1,file='rk1.dat',status='replace')
          y(0)=0.0
          ta=0.01
          h=0.01
          write(*,*)y(0) 
          do i=1,100
           j=i-1
           t=j*ta
           x=t+h  
           y(i)=y(i-1)+(h/2.)*((f(t,y(i-1)))+f(t+h,y(i-1)+h*f(t,y(i-1))))
           !tiempo, solución de y, solución analítica
           print*, x,y(i),1./25.*exp(-2.*x)*(exp(5.*x)*(5.*x-1.)+1.) 
           write(1,*)x,y(i),1./25.*exp(-2.*x)*(exp(5.*x)*(5.*x-1.)+1.) 
          enddo
          end
          function f(t,y)
          real t,y
           f=((t*exp(3.*t))-(2.*y))
          end















