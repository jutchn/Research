addpath /projectnb/zhaohcp/syan1019/scripts/gifti-1.8
fn = getfn('/projectnb/zhaohcp/MMPOutput', 'gii$');
for k = 1:length(fn) 
    g = gifti(fn(1,k));
    pt = extractBetween(fn(1,k),1,'.pt');
    all = strcat(pt,'.txt');
    all = char(all);
    dlmwrite(all,g.cdata);
end

