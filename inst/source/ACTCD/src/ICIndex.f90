subroutine ICIndex(IC_result,k,length,p,a,w)


  implicit none
  integer,intent(in)::k
  integer,intent(in):: length
  integer I,J,m1,m2,n,q,IC1,IC2,IC3,sum1,sum2,col
  integer,intent(out):: IC_result(length,1)
  integer,intent(in)::p(length,2**k)
  integer,intent(in)::a(2**k,k)
  integer temp(2**k,k)
  doubleprecision,intent(in)::w(2**k,k)

 DO I=1,ubound(p,1) !for each row from 1 to 1680384
    IC1=0
    IC2=0
    IC3=0
    DO J=1,ubound(p,2)         !according to each row of p
        temp(J,:)=a(p(I,J),:)  !create a permutated matrix
    enddo                      !which is called temp
    do m1=1,ubound(temp,1) !start to compute the first part of IC
       do n=1,(ubound(temp,2)-1)      !n is from 1 to col_num of temp-1
            do q=(n+1),ubound(temp,2)  !q is from n+1 to col_num of temp
                if ((temp(m1,n)/=temp(m1,q)).and.(((w(m1,n)-w(m1,q))*(temp(m1,n)-temp(m1,q))<=0))) then !the first part of IC
                    IC1=IC1+1
                endif
            enddo
        enddo
    enddo !finish the computation of the first part of IC
   do m1=1,(ubound(temp,1)-1) !start to compute the second part of IC
        do m2=(m1+1),ubound(temp,1)
            do n=1,ubound(temp,2)
                if ((temp(m1,n)/=temp(m2,n)).and.(((w(m1,n)-w(m2,n))*(temp(m1,n)-temp(m2,n))<=0))) then
                    IC2=IC2+1
                endif
            enddo
        enddo
    enddo !finish computing the second part of IC

    do m1=1,(ubound(temp,1)-1)  !m1 is from 1 to row_num of temp-1
        !=========compute rowsum of m1 row=========???
        sum1=0
        do col=1,ubound(temp,2)
            sum1=sum1+temp(m1,col)
        enddo

        do m2=(m1+1),ubound(temp,1) !m2 is from m1+1 to row_num of temp
            !=========compute rowsum of m2 row=========???
            sum2=0
            do col=1,ubound(temp,2)
                sum2=sum2+temp(m2,col)
            enddo

            if (sum1/=sum2) then !||alpha|| is not equal to ||alpha*||
                do q=1,ubound(temp,2)
                    if (((temp(m1,q)==1).and.(temp(m2,q)==1)).and.((sum1-sum2)*(w(m1,q)-w(m2,q))<0)) then
                        IC3=IC3+1
                    endif
                enddo
            endif
        enddo
    enddo !finsh computing the third part of IC
    IC_result(I,1)=IC1+IC2+IC3
  enddo

end subroutine