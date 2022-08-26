% eg.
% load MUTAG
 load ENZYMES
% load DD
% load NCI1
% load NCI109

bestKernelResults = runTest(ENZYMES, lenzymes, 2);

% =========================================================================

% runTest: example of a function to run the svm tests
function [result] = runTest(dataset, labels, minRefinements)
    h = 10;
    
    if minRefinements > 0
        c = [2^-11, 2^-9, 2^-7, 2^-5, 2^-3, 2^-1, 2^1, 2^3, 2^5, 2^7, 2^9, 2^11];
    else
        c = [2^-7, 2^-5, 2^-3, 2^-1, 2^1, 2^3, 2^5, 2^7];
    end

    gram = WL(dataset, h, 1);
    %gram = WLedge(dataset, h, 1);
    %gram = WLspdelta(dataset, h, 1, 0);

    temp = tuneGramMatrix(gram, labels, c, minRefinements, 5);
    result.tuningAccuracy = temp(1);
    result.bestCost = temp(2);
    result.bestHyperparam = temp(3);
    testErr = trainClassifier(gram, labels, temp(2), temp(3));
    result.meanTestAccuracy = mean(testErr);
end


% trainClassifier: Finds the combination of cost and hyperparameter that
% results in the best CV error. minRefinements controls the minimum number
% of cost refinements performed. When set to 0, the SVMs are only trained
% on the given costs c. When greater than 0, cost refinement is turned on,
% and will continue to occur until (minRefinements) number of cost vectors 
% refinements produced less than 0.05 performance increase 
function result = tuneGramMatrix(gram, labels, c, minRefinements, numFolds)
    cost           = c;
    numCost        = length(cost);
    numHyperparams = length(gram);

    epsilon        = 0.05;

    bestHyperparam = -1;
    bestModel      = -1; 
    bestCost       = -1;

    if minRefinements == 0
        endEarly = true;
    else
        endEarly = false;
    end

    count = 0;
    while count <= minRefinements
        prevBestModel  = bestModel;
        bestModel      = -1; 
        bestCostIndex  = 1;
        bestHyperparam = 1;
     
        for i=1:numHyperparams
            scaledGram = scaleToUnitInterval(cell2mat(gram(i)));
            for j=1:numCost
                augGram = [(1:length(scaledGram))', scaledGram];
                model   = svmtrain(labels, augGram, strcat(['-t 4 -v ' num2str(numFolds) ' -q -c ' num2str(cost(j))]));
                if model > bestModel
                    bestModel      = model;
                    bestHyperparam = i;
                    bestCostIndex  = j;
                    bestCost       = cost(j);
                end
            end       
        end

        if abs(bestModel - prevBestModel) < epsilon
            count = count + 1;
        end

        if endEarly
            break;
        end

        cost = autoTuning(cost, bestCostIndex);
    end

    result = [bestModel bestCost bestHyperparam];
end


% trainClassifier: trains 10 SVMs using the given gram matrix on 90% of the 
% data, and tests accuracy on the remaining 10%. Returns a vector
% containing the ten accuracys calculations
function [tc] = trainClassifier(gram, labels, cost, hyperparam)
    n            = length(labels);
    trainingSize = ceil(n * (9/10));
    testingSize  = n - trainingSize;
    r            = randperm(n);
    lk           = labels(r);
    K            = gram{hyperparam}(r,r);
    tc           = zeros(1,10);

    for i=1:10    
        K_current  = K([i*testingSize+1:size(K,2), 1:(i-1)*testingSize, (i-1)*testingSize + 1:i*testingSize], [i*testingSize+1:size(K,2), 1:(i-1)*testingSize, (i-1)*testingSize + 1:i*testingSize]);  
        lk_current = lk([i*testingSize+1:size(K,2), 1:(i-1)*testingSize, (i-1)*testingSize + 1:i*testingSize]); 
        
        scaledGram = scaleToUnitInterval(K_current);
        augGram    = [(1:length(scaledGram))', scaledGram];
        
        model = svmtrain(lk_current(1:trainingSize,1), augGram(1:trainingSize,1:trainingSize+1), strcat(['-t 4 -q -c ' num2str(cost)]));
        [predict_label, accuracy, dec_values] = svmpredict(lk_current(trainingSize + 1:size(K,1),1), augGram(trainingSize + 1:size(K,1), 1:trainingSize + 1), model);
        
        tc(i) = accuracy(1);
    end
end


% autoTuning: creates a new cost vector based on the neighbourhood
% of the previous best cost.
function new = autoTuning(cost, bestIndex)
    n   = length(cost);
    new = zeros(1, n);
    mx  = 0;
    mn  = 0;

    mid = false;

    if bestIndex == 1
        mn = cost(bestIndex);
        mx = cost(bestIndex + 2);
    elseif bestIndex == n
        mn = cost(bestIndex - 2);
        mx = cost(bestIndex);
    else
        mn  = cost(bestIndex - 1);
        mx  = cost(bestIndex + 1);
        mid = true;
    end

    temp = [mn, mx, cost(bestIndex)];
    logVals = log2(temp);
    
    for i=1:n
        new(i) = logVals(1) + (i - 1)*(logVals(2) - logVals(1))/(n-1);
    end

    % Add previous best cost back into cost vector 
    if mid
        i     = 2;
        found = false;
        while ~found && i <= n
            if new(i) > logVals(3)
                new(i-1) = logVals(3);
                found    = true;
            else
                i = i + 1; 
            end
        end
    end

    new = 2.^new;
end


% scaleToUnitInterval: scales entries of a matrix to the unit interval
function g = scaleToUnitInterval(gram)
    mx = max(gram, [], 'all');
    mn = min(gram, [], 'all');
    s  = 1/(mx - mn);
    b  = -(mn)/(mx - mn);
    g  =  s * gram + b;
end
