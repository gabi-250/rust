fn iter_sum() -> u64 {
    let v = vec![3,4,7,12,19,28,39,52,67,84,103,124,147,172,199,228,259,292,327,364,403,444,487,532,579,628,679,732,787,844,903,964,1027,1092,1159,1228,1299,1372,1447,1524,1603,1684,1767,1852,1939,2028,2119,2212,2307,2404,2503,2604,2707,2812,2919,3028,3139,3252,3367,3484,3603,3724,3847,3972,4099,4228,4359,4492,4627,4764,4903,5044,5187,5332,5479,5628,5779,5932,6087,6244,6403,6564,6727,6892,7059,7228,7399,7572,7747,7924,8103,8284,8467,8652,8839,9028,9219,9412,9607,9804,10003,10204,10407,10612,10819,11028,11239,11452,11667,11884,12103,12324,12547,12772,12999,13228,13459,13692,13927,14164,14403,14644,14887,15132,15379,15628,15879,16132,16387,16644,16903,17164,17427,17692,17959,18228,18499,18772,19047,19324,19603,19884,20167,20452,20739,21028,21319,21612,21907,22204,22503,22804,23107,23412,23719,24028,24339,24652,24967,25284,25603,25924,26247,26572,26899,27228,27559,27892,28227,28564,28903,29244,29587,29932,30279,30628,30979,31332,31687,32044,32403,32764,33127,33492,33859,34228,34599,34972,35347,35724,36103,36484,36867,37252,37639,38028,38419,38812,39207,39604,40003,40404,40807,41212,41619,42028,42439,42852,43267,43684,44103,44524,44947,45372,45799,46228,46659,47092,47527,47964,48403,48844,49287,49732,50179,50628,51079,51532,51987,52444,52903,53364,53827,54292,54759,55228,55699,56172,56647,57124,57603,58084,58567,59052,59539,60028,60519,61012,61507,62004,62503,63004,63507,64012,64519,65028,65539,66052,66567,67084,67603,68124,68647,69172,69699,70228,70759,71292,71827,72364,72903,73444,73987,74532,75079,75628,76179,76732,77287,77844,78403,78964,79527,80092,80659,81228,81799,82372,82947,83524,84103,84684,85267,85852,86439,87028,87619,88212,88807,89404,90003,90604,91207,91812,92419,93028,93639,94252,94867,95484,96103,96724,97347,97972,98599,99228,99859,100492,101127,101764,102403,103044,103687,104332,104979,105628,106279,106932,107587,108244,108903,109564,110227,110892,111559,112228,112899,113572,114247,114924,115603,116284,116967,117652,118339,119028,119719,120412,121107,121804,122503,123204,123907,124612,125319,126028,126739,127452,128167,128884,129603,130324,131047,131772,132499,133228,133959,134692,135427,136164,136903,137644,138387,139132,139879,140628,141379,142132,142887,143644,144403,145164,145927,146692,147459,148228,148999,149772,150547,151324,152103,152884,153667,154452,155239,156028,156819,157612,158407,159204,160003,160804,161607,162412,163219,164028,164839,165652,166467,167284,168103,168924,169747,170572,171399,172228,173059,173892,174727,175564,176403,177244,178087,178932,179779,180628,181479,182332,183187,184044,184903,185764,186627,187492,188359,189228,190099,190972,191847,192724,193603,194484,195367,196252,197139,198028,198919,199812,200707,201604,202503,203404,204307,205212,206119,207028,207939,208852,209767,210684,211603,212524,213447,214372,215299,216228,217159,218092,219027,219964,220903,221844,222787,223732,224679,225628,226579,227532,228487,229444,230403,231364,232327,233292,234259,235228,236199,237172,238147,239124,240103,241084,242067,243052,244039,245028,246019,247012,248007,249004];
    let mut sum = 0;
    let mut i = 0;
    while i < v.len() {
        sum = sum + v[i];
        i = i + 1;
    }
    sum
}

fn main() {
    println!("iter_sum: {:?}", iter_sum());
}
