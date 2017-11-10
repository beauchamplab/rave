function[V,U,D,Xhat,bicmat,optlams] = sparse_pca(x,K,lamv,startu,startv,posv,maxit)
%function to compute sparse pca - via shen and huang method
%note this function uses BIC to select optimal lambdas
%
%inputs:
%x - a n x p data matrix
%K - number of sprase PCs to extract
%lamv - a vector of possible lambda values to try - lambda controls
%sparsity of v
%startu - a matrix of n x K initializations for U - note this is
%optional and can be set to 0 to initalize to the SVD solution
%startv - a matrix of p x K initializations for V - note this is
%optional and can be set to 0 to initalize to the SVD solution
%posv - an indicator for sparse non-negative PCA, 1 = yes, 0 = no
%maxit - max number of iterations 1e3 is sufficient
%
%outputs:
%V - a p x K matrix whose columns are the sparse PC loadings
%U - a n x K matrix whose columns are the PCs
%D - a K-length vector of singular values
%Xhat - a n x p matrix of residuals after subtracting out the first
%K sparse PCs
%bicmat - a length(lamv) x K matrix of BIC values for each
%component and each lambda value
%optlmas - a K-length vector of the optimal lambda selected for
%each component

U = []; V = []; D = [];
Xhat = x;
[n,p] = size(x);
r = length(lamv); setr = 1:r;
bicmat = zeros(r,K); optlams = [];
Us = zeros(n,r); Vs = zeros(p,r); ds = zeros(r,1);
for k=1:K
    for j=1:r
        if norm(startu)==0
            [u,d,v] = svds(Xhat,1);
            %u = randn(n,1);
            %v = randn(p,1);
        else
            u = startu(:,k);
            v = startv(:,k);
        end
        ind = 1; iter = 0; thr = 1e-6;
        obj = u'*Xhat*v - lamv(j)*sum(abs(v));
        while ind>thr & maxit>iter
            oldu = u; oldv = v; oldo = obj;
            vhat = soft_thr(Xhat'*u,lamv(j),posv);
            if norm(vhat)==0
                v = zeros(p,1);
                u = zeros(n,1);
            else
                v = vhat/norm(vhat);
                uhat = Xhat*v; 
                u = uhat/norm(uhat);
            end
            %ind = norm(oldu - u)/norm(oldu) + norm(oldv - v)/norm(oldv);
            obj = u'*Xhat*v - lamv(j)*sum(abs(v));
            ind = abs((obj - oldo)/oldo);
            iter = iter + 1;
        end
        d = u'*Xhat*v;
        df = sum(v~=0);
        xr = sum(sum((Xhat - d*u*v').^2));
        bicmat(j,k) = log( xr / (n*p) ) + (log(n*p)/(n*p)).*df;
        Us(:,j) = u; Vs(:,j) = v; ds(j) = d;
    end
    ind = bicmat(:,k)==min(bicmat(:,k));
    if sum(ind)>1
        ind = min(setr(ind));
    end
    optlams = [optlams; lamv(ind)];
    U = [U Us(:,ind)]; V = [V Vs(:,ind)]; D(k) = ds(ind);
    clear Us; clear Vs;
    Xhat = Xhat - D(k)*U(:,k)*V(:,k)';
end

end





