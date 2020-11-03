
binary_m = bmatrices;
weighted_m = wmatrices;

[numRows,numCols] = size(binary_m);

numRegions = numRows
numSubjects = numCols/numRows

binary = zeros(numRegions, numRegions, numSubjects);
weighted = zeros(numRegions, numRegions, numSubjects);

%parsing subject-adj matrices
for i = 1:numSubjects
    for j = 1:numRegions
        for k = 1:numRegions   
            binary(j,k,i) = binary_m(j,k + (i-1)*numRegions);
            weighted(j,k,i) = weighted_m(j,k + (i-1)*numRegions);
        end
    end
end

%topological measures

b_local_eff = zeros(numSubjects,numRegions);
b_global_eff = zeros(numSubjects,1);
b_degree = zeros(numSubjects,numRegions);
b_clust_coef = zeros(numSubjects,numRegions);
b_char_path = zeros(numSubjects,1);
b_rich_club = cell(numSubjects,1);

w_local_eff = zeros(numSubjects,numRegions);
w_global_eff = zeros(numSubjects,1);
w_degree = zeros(numSubjects,numRegions);
w_clust_coef = zeros(numSubjects,numRegions);
w_char_path = zeros(numSubjects,1);
w_rich_club = cell(numSubjects,1);


for i = 1:numSubjects
   b_local_eff(i,:) = transpose(efficiency_bin(binary(:,:,i),1));
   b_global_eff(i,:) = efficiency_bin(binary(:,:,i),0);
   b_degree(i,:) = degrees_und(binary(:,:,i));
   b_clust_coef(i,:) = transpose(clustering_coef_bu(binary(:,:,i)));
   b_char_path(i,:) = charpath(binary(:,:,i));
   b_rich_club{i,:} = rich_club_bu(binary(:,:,i));

   w_local_eff(i,:) = transpose(efficiency_wei(weighted(:,:,i),1));
   w_global_eff(i,:) = efficiency_wei(weighted(:,:,i),0);
   w_degree(i,:) = degrees_und(weighted(:,:,i));
   w_clust_coef(i,:) = transpose(clustering_coef_wu(weighted(:,:,i)));
   w_char_path(i,:) = charpath(weighted(:,:,i));
   w_rich_club{i,:} = rich_club_wu(weighted(:,:,i));
end

%{ 
b_rich_tab = cell2table(b_rich_club(:,1))
writetable(b_rich_tab,'b_rich.csv')

w_rich_tab = cell2table(w_rich_club(:,1))
writetable(w_rich_tab,'w_rich.csv')


binary_top = horzcat(b_degree,b_clust_coef,b_char_path,b_local_eff,b_global_eff);
weighted_top = horzcat(w_degree,w_clust_coef,w_char_path,w_local_eff,w_global_eff);

writematrix(binary_top,'b_top.csv')
writematrix(weighted_top,'w_top.csv')
%}