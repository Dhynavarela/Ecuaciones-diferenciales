          !Método de Taylor para la función y'=t*exp(3t)-2y, de 0 a 1 
          real(kind=4):: y(0:100),ta,t,dt,h,x
          open(1,file='tayl.dat',status='replace')
          y(0)=0.0
          print*, y(0)
          do i=1,100
           j=i-1
           ta=0.01
           t=j*ta
           dt=0.01
           h=t+dt  
           y(i)=y(i-1)+(t*exp(3.*t)-2.*y(i-1))*dt+(exp(3*t)+3.*t*exp(3.*t)-2.*t*exp(3*t)+4.*y(i-1))*((dt**2)/2.)+&
           (4.*exp(3.*t)+7.*t*exp(3.*t)-8.*y(i-1))*((dt**3)/3.)
           !tiempo, solución de y, solución analítica
           print*, h,y(i), 1./25.*exp(-2.*h)*(exp(5.*h)*(5.*h-1.)+1.) 
           write(1,*) h,y(i),1./25.*exp(-2.*h)*(exp(5.*h)*(5.*h-1.)+1.)
          enddo
          end
          

