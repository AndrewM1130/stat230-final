mata:
    data=.; st_view(data,.,.); N=rows(data); e=data[.,4]; X=data[.,5..cols(data)];
    W=J(N,N,0); for(j=1;j<=N;j++) for(i=1;i<=N;i++) W[i,j] = sqrt((data[i,2]-data[j,2])^2+(data[i,3]-data[j,3])^2);
    H=X; D=H*invsym(H'*H)*H'*X; b=-H*invsym(D'*D)*D'*H*invsym(H'*H)*D'*(W+W')*e; S2=e'*e/N;
    I = abs(e'*W*e/sqrt(S2^2*trace(W*W+W'*W)+S2*b'*b))
    st_numscalar("moransI",I)
end
