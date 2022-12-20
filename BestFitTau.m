
function [BestTau] = BestFitTau (observed, payoffarray, kvalue)
tic
MSEValues = [zeros(1,5001)];

Tauvalues = 0:.001:5;

    parfor index = 1:numel(Tauvalues)

%previous loop cannot depend on another
%puta ll values in fiant array, find smallest
%take index and find best tau
    
Tau = Tauvalues(index);

    



modelans = CogHierSol(payoffarray,Tau,kvalue);
model = modelans{1,1};

MSE= sum((observed-model).^2);

MSEValues(index) = MSE;
    
    
    end

[M,I] = min(MSEValues);

BestTau = Tauvalues(I);
toc
end
