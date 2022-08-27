*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


CLASS lcl_input DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.
    DATA mt_input TYPE stringtab.
ENDCLASS.


CLASS lcl_input IMPLEMENTATION.
  METHOD constructor.
    mt_input = VALUE #(
          ( |departure location: 49-627 or 650-970| )
          ( |departure station: 26-400 or 413-965| )
          ( |departure platform: 28-375 or 390-971| )
          ( |departure track: 45-393 or 400-954| )
          ( |departure date: 32-760 or 783-955| )
          ( |departure time: 25-420 or 439-961| )
          ( |arrival location: 40-573 or 582-964| )
          ( |arrival station: 42-839 or 857-964| )
          ( |arrival platform: 26-89 or 105-970| )
          ( |arrival track: 30-502 or 510-952| )
          ( |class: 39-882 or 888-956| )
          ( |duration: 35-352 or 371-971| )
          ( |price: 45-467 or 486-970| )
          ( |route: 32-746 or 759-972| )
          ( |row: 45-461 or 467-955| )
          ( |seat: 29-790 or 809-963| )
          ( |train: 41-445 or 461-952| )
          ( |type: 40-683 or 701-974| )
          ( |wagon: 38-111 or 127-963| )
          ( |zone: 28-226 or 234-951| )
          ( || )
          ( |your ticket:| )
          ( |107,109,163,127,167,157,139,67,131,59,151,53,73,83,61,89,71,149,79,137| )
          ( || )
          ( |nearby tickets:| )
          ( |910,308,590,919,735,895,709,238,269,618,606,437,541,938,912,811,330,80,283,210| )
          ( |256,680,352,64,818,63,168,78,564,179,859,908,724,306,312,719,624,866,929,451| )
          ( |533,889,715,87,902,258,295,150,837,199,160,286,785,927,129,656,888,401,142,926| )
          ( |141,915,162,138,283,528,319,84,902,587,139,278,201,111,300,176,394,333,786,937| )
          ( |239,263,85,78,317,472,278,713,490,875,225,608,943,106,567,789,891,713,675,881| )
          ( |943,273,488,306,839,283,710,178,195,831,720,496,558,895,108,918,491,240,217,485| )
          ( |861,730,203,555,904,292,213,163,872,569,123,310,522,731,337,308,600,400,298,661| )
          ( |608,270,738,682,210,139,662,196,495,132,494,282,824,678,921,887,226,264,915,172| )
          ( |821,626,72,199,822,824,784,264,260,295,544,418,583,336,57,783,12,601,558,245| )
          ( |334,254,872,570,109,79,786,587,445,419,149,698,273,566,860,77,561,944,722,73| )
          ( |705,318,289,282,349,231,709,567,912,783,312,602,129,721,831,922,569,893,225,176| )
          ( |620,444,520,232,570,467,668,220,178,676,787,830,941,861,937,614,495,864,243,714| )
          ( |344,173,875,88,73,254,624,623,627,61,61,906,326,234,455,86,743,163,785,55| )
          ( |514,655,137,589,857,605,215,924,819,209,289,589,179,210,153,947,289,506,135,941| )
          ( |296,519,89,415,717,832,550,127,441,906,846,132,609,109,711,335,374,819,292,216| )
          ( |745,979,822,701,288,158,170,263,878,334,606,273,931,190,284,915,259,654,174,560| )
          ( |933,501,735,872,290,335,157,995,892,263,219,811,897,662,172,206,589,529,214,785| )
          ( |618,730,817,602,336,554,796,760,241,824,569,349,519,197,839,420,788,866,741,84| )
          ( |949,286,309,571,890,900,890,107,940,347,301,447,400,569,371,889,598,337,872,225| )
          ( |544,494,931,834,518,159,858,519,659,929,245,325,672,148,755,540,511,518,263,200| )
          ( |204,848,653,532,877,352,568,949,534,587,86,715,325,247,444,315,282,511,467,254| )
          ( |253,617,605,906,212,934,787,275,898,563,906,566,678,708,246,683,320,683,136,117| )
          ( |661,62,137,512,785,536,914,443,896,875,657,767,400,838,190,828,868,900,534,737| )
          ( |317,85,665,200,551,69,288,84,534,190,738,530,141,413,235,446,150,127,270,76| )
          ( |523,302,664,496,606,762,150,563,744,340,306,814,205,491,561,80,899,181,561,158| )
          ( |536,534,652,835,555,825,831,739,536,610,163,907,70,940,309,129,160,256,590,998| )
          ( |334,670,656,829,515,520,927,534,789,790,598,348,941,606,816,575,906,826,870,787| )
          ( |173,600,348,202,627,55,179,522,735,16,920,566,821,599,585,444,151,606,313,208| )
          ( |87,274,444,864,419,947,880,530,537,650,456,414,616,874,716,70,735,863,551,390| )
          ( |680,536,539,875,821,931,833,286,109,585,77,128,895,681,129,593,273,189,566,782| )
          ( |135,602,156,246,278,714,567,682,247,153,947,569,159,675,864,946,147,980,172,199| )
          ( |183,331,131,716,918,64,239,598,512,544,752,337,948,516,270,136,110,349,327,319| )
          ( |904,129,652,865,390,188,262,597,416,50,255,679,897,577,165,592,210,413,219,75| )
          ( |870,197,292,105,148,679,181,627,570,82,133,291,530,319,585,560,815,650,657,401| )
          ( |312,739,283,276,921,493,177,65,895,864,869,126,212,545,930,925,650,320,155,172| )
          ( |151,717,605,183,79,923,145,712,514,223,571,410,832,135,272,861,157,162,745,924| )
          ( |59,760,136,582,148,181,704,743,547,651,820,880,694,169,522,307,759,923,257,551| )
          ( |255,215,116,587,746,129,292,828,313,881,610,496,739,888,564,110,241,561,531,51| )
          ( |625,78,921,573,879,721,734,947,60,702,131,753,160,535,190,272,191,566,245,673| )
          ( |413,874,382,249,882,194,291,241,948,740,420,85,911,62,69,551,880,333,896,322| )
          ( |672,259,820,730,710,323,618,147,948,333,759,616,247,896,258,875,229,74,864,215| )
          ( |621,54,540,733,867,164,171,941,991,152,81,294,259,568,861,307,218,822,706,598| )
          ( |183,216,528,226,674,301,333,555,297,159,671,665,252,192,656,730,616,278,119,149| )
          ( |418,188,749,861,467,838,73,702,656,683,150,832,727,337,56,616,332,151,282,731| )
          ( |268,141,352,269,270,729,927,870,3,905,932,179,214,937,896,165,53,736,934,817| )
          ( |908,238,512,226,141,191,713,829,267,287,170,144,791,516,621,682,944,245,193,860| )
          ( |293,147,562,677,61,668,526,210,539,735,650,843,208,234,413,495,127,171,707,50| )
          ( |714,490,606,785,725,716,671,190,306,333,168,220,896,494,624,189,76,814,253,855| )
          ( |540,593,593,621,337,906,710,706,809,923,896,517,529,585,545,606,551,22,134,220| )
          ( |862,657,883,135,670,600,141,709,607,827,920,658,57,625,662,613,669,209,860,824| )
          ( |621,919,824,487,619,942,146,826,992,299,138,784,373,784,203,488,677,535,783,658| )
          ( |201,139,60,945,287,132,500,490,219,202,605,602,884,270,313,679,208,191,136,839| )
          ( |418,583,719,868,284,608,879,185,308,878,139,144,314,623,947,681,13,930,145,701| )
          ( |85,664,908,589,853,500,293,889,948,108,278,333,439,225,549,209,50,719,325,247| )
          ( |148,602,549,70,287,238,106,291,938,741,152,444,265,542,275,663,149,436,165,337| )
          ( |444,546,606,580,500,325,560,834,921,327,312,326,145,709,900,663,707,931,937,879| )
          ( |70,669,267,621,250,178,909,601,837,892,278,334,835,737,213,983,59,71,500,935| )
          ( |588,558,350,852,555,258,544,345,487,336,192,819,181,827,827,598,288,105,152,170| )
          ( |797,522,865,613,202,500,872,326,548,499,334,931,243,306,272,304,414,106,602,559| )
          ( |298,149,687,596,604,811,787,52,185,672,912,787,613,514,212,618,624,131,786,176| )
          ( |874,295,857,82,442,81,624,594,864,84,300,285,382,214,839,71,870,704,299,255| )
          ( |208,617,163,88,307,209,721,927,702,946,715,387,597,169,151,276,216,915,173,813| )
          ( |916,892,530,443,937,507,175,234,373,290,944,740,868,79,241,928,658,498,674,73| )
          ( |487,912,391,740,200,512,577,58,176,335,898,920,339,289,899,288,718,814,860,655| )
          ( |86,903,375,279,284,538,501,935,78,342,500,630,570,916,156,729,920,257,931,79| )
          ( |213,301,111,327,490,211,534,393,873,600,734,50,933,662,265,20,152,214,893,311| )
          ( |452,164,826,544,859,912,413,251,350,722,616,293,142,948,904,935,137,87,208,77| )
          ( |615,76,650,500,582,535,719,530,823,317,598,65,681,706,626,126,266,598,548,310| )
          ( |838,834,921,167,996,85,734,613,257,822,746,288,535,894,60,592,721,624,286,893| )
          ( |543,169,215,193,542,832,375,156,527,439,166,553,374,206,150,311,611,989,891,316| )
          ( |493,178,274,152,977,417,372,517,719,561,63,154,150,659,878,154,58,255,299,223| )
          ( |826,284,559,922,498,705,713,616,310,83,315,916,602,535,578,322,310,50,681,69| )
          ( |826,201,324,532,911,132,790,618,549,272,545,626,703,499,743,490,312,180,14,134| )
          ( |677,865,916,560,325,567,246,836,788,192,184,683,846,661,208,790,526,675,568,461| )
          ( |916,490,602,179,275,218,984,786,737,347,418,315,149,342,83,351,658,174,722,658| )
          ( |545,232,716,677,221,596,107,111,251,732,149,821,598,511,127,169,215,857,58,62| )
          ( |947,325,283,906,110,220,220,250,937,107,173,679,318,861,681,627,575,660,323,292| )
          ( |881,351,300,199,542,107,135,343,738,225,729,936,778,821,155,516,269,722,152,154| )
          ( |574,943,167,614,286,467,343,255,681,303,306,734,942,268,926,882,59,148,897,526| )
          ( |296,902,786,833,531,705,146,916,81,512,343,871,175,523,227,286,297,499,927,139| )
          ( |233,551,414,87,290,545,732,186,502,249,328,527,706,584,63,910,916,925,712,682| )
          ( |893,571,18,921,160,897,324,525,740,284,305,315,193,811,526,652,74,502,445,523| )
          ( |920,249,582,242,616,281,151,350,260,815,320,873,241,219,821,384,719,915,300,350| )
          ( |588,907,600,291,497,84,785,526,199,413,327,595,308,405,939,348,85,623,708,729| )
          ( |820,199,279,241,938,219,265,711,566,177,554,156,111,139,391,908,795,174,160,562| )
          ( |107,652,893,414,518,816,200,240,187,80,223,683,601,505,68,445,299,420,936,561| )
          ( |521,741,317,192,57,948,371,261,564,489,209,237,130,799,166,788,487,522,128,249| )
          ( |393,305,177,535,703,70,65,669,171,557,890,862,921,530,542,299,251,849,869,537| )
          ( |142,223,329,244,930,328,53,880,330,218,560,476,193,224,51,533,149,494,315,863| )
          ( |616,371,317,866,651,74,263,917,235,909,134,734,3,522,535,659,608,652,307,934| )
          ( |657,258,315,599,894,279,615,488,491,827,723,325,50,597,272,889,267,291,980,302| )
          ( |255,936,925,874,59,935,668,515,605,553,785,528,84,655,859,920,597,13,539,598| )
          ( |65,895,939,716,203,835,813,941,882,178,304,465,726,87,179,941,896,822,668,328| )
          ( |912,933,609,329,207,711,344,85,173,296,672,509,243,439,209,650,325,941,828,530| )
          ( |414,890,309,347,611,105,251,930,744,487,137,349,169,134,177,396,832,604,160,167| )
          ( |549,608,213,284,390,517,732,59,151,497,70,626,737,79,661,917,134,807,523,514| )
          ( |316,107,608,620,65,400,614,319,743,850,279,147,526,599,170,939,897,611,651,180| )
          ( |159,58,831,336,247,523,525,146,67,905,322,19,905,810,670,256,898,705,589,939| )
          ( |935,138,767,907,595,839,151,898,131,681,584,947,540,279,280,279,250,556,573,608| )
          ( |617,12,709,301,544,148,729,129,522,610,163,443,610,912,415,590,831,906,142,563| )
          ( |500,652,739,136,295,172,610,56,929,839,604,142,597,491,933,434,926,708,157,820| )
          ( |719,608,65,148,552,838,499,239,60,188,145,912,145,228,150,167,759,184,309,874| )
          ( |287,211,209,908,516,900,902,676,374,420,829,310,411,556,138,76,936,272,415,343| )
          ( |68,467,52,657,194,305,312,717,488,600,937,945,872,749,165,515,316,297,596,722| )
          ( |867,573,624,932,912,259,839,319,907,257,915,811,603,659,556,591,488,54,109,976| )
          ( |293,140,740,5,329,601,181,829,87,665,663,214,278,594,720,746,277,200,327,906| )
          ( |182,525,188,868,564,107,55,214,511,172,818,896,659,220,0,625,322,50,270,260| )
          ( |215,870,607,130,936,941,304,731,728,153,813,218,915,438,620,536,572,733,935,85| )
          ( |832,566,725,138,667,183,609,821,815,50,785,194,516,482,500,708,891,339,66,584| )
          ( |291,59,372,135,186,260,856,816,137,554,723,931,820,205,299,539,601,335,264,672| )
          ( |570,518,681,447,65,175,342,299,835,73,501,514,930,295,521,598,873,351,71,512| )
          ( |170,53,308,573,873,627,224,245,599,708,323,346,442,55,942,628,352,273,443,173| )
          ( |821,515,494,309,559,167,521,890,787,306,81,164,666,80,325,542,208,735,996,510| )
          ( |933,826,658,601,826,256,341,566,871,203,535,607,508,561,151,626,266,876,55,935| )
          ( |89,817,349,280,131,181,908,550,712,171,931,491,809,902,502,753,491,140,263,784| )
          ( |344,541,474,866,861,570,525,491,657,51,107,331,75,349,297,220,605,785,420,660| )
          ( |336,877,160,441,291,197,390,307,566,441,300,274,144,530,298,234,336,581,313,930| )
          ( |871,664,241,909,739,598,164,571,910,244,238,664,743,557,855,445,872,932,86,943| )
          ( |727,138,410,601,496,869,343,662,192,374,620,545,134,809,313,323,440,615,319,73| )
          ( |316,249,347,737,249,549,56,909,487,167,445,302,723,729,930,811,914,86,233,682| )
          ( |990,273,559,514,724,349,924,668,176,163,289,244,314,282,860,169,134,836,238,513| )
          ( |707,105,545,563,58,567,222,739,86,556,239,445,168,309,247,152,109,83,470,827| )
          ( |823,572,892,719,501,746,552,414,572,109,316,538,820,583,194,792,249,318,492,281| )
          ( |106,869,734,664,196,169,135,57,548,267,147,243,920,743,325,296,139,227,277,650| )
          ( |543,760,514,328,824,324,348,341,607,891,229,486,191,789,937,898,650,210,671,569| )
          ( |732,559,167,906,393,936,869,178,269,173,615,557,916,901,716,136,341,727,508,190| )
          ( |22,889,298,542,323,296,889,599,63,744,743,719,333,413,51,627,304,923,615,391| )
          ( |867,154,417,308,911,550,228,816,910,571,827,936,80,857,590,141,943,489,297,346| )
          ( |814,606,135,600,728,904,70,587,209,616,701,573,535,553,490,612,936,526,741,991| )
          ( |679,309,290,187,52,736,875,315,929,182,200,799,601,584,290,416,607,326,910,605| )
          ( |674,816,872,329,185,809,121,809,817,298,925,830,84,815,106,819,305,890,416,680| )
          ( |740,190,415,58,666,10,128,250,565,86,143,863,290,679,249,294,77,833,87,138| )
          ( |560,400,829,652,284,796,152,420,319,175,178,168,872,164,252,83,730,338,81,547| )
          ( |565,679,155,603,673,922,600,612,323,927,578,266,714,587,160,868,937,898,321,862| )
          ( |314,657,53,307,390,555,298,589,215,725,859,467,722,447,417,71,865,873,68,291| )
          ( |73,608,229,271,521,192,554,80,896,550,877,930,542,738,111,729,108,532,615,889| )
          ( |288,902,332,673,669,931,921,889,237,655,872,830,335,325,444,322,630,441,670,724| )
          ( |258,670,941,938,600,176,391,274,524,263,899,511,229,623,236,863,729,875,306,295| )
          ( |655,105,414,935,309,861,672,538,66,228,510,890,863,679,584,461,66,283,203,570| )
          ( |522,557,907,261,207,672,681,937,713,298,140,585,750,252,528,461,246,879,917,620| )
          ( |518,564,153,678,540,835,732,561,825,922,247,907,250,257,106,495,770,252,528,553| )
          ( |889,890,678,738,267,378,147,444,71,831,837,133,703,933,440,497,293,272,310,893| )
          ( |829,838,264,662,534,419,742,555,76,190,127,656,350,833,743,306,288,14,391,555| )
          ( |555,946,251,315,219,810,662,170,213,822,400,314,278,529,783,223,194,187,979,162| )
          ( |677,165,826,811,137,736,374,920,56,442,188,281,55,152,793,343,898,597,600,661| )
          ( |300,908,496,392,839,110,664,949,823,545,590,257,168,889,201,627,348,157,536,505| )
          ( |73,936,801,55,486,598,937,826,857,204,148,341,301,320,880,209,144,593,785,262| )
          ( |583,211,90,54,160,442,680,418,109,608,860,299,612,512,606,286,819,174,155,512| )
          ( |192,225,53,683,183,825,323,607,873,893,222,670,343,120,944,55,285,157,720,673| )
          ( |282,314,679,130,878,529,601,836,948,174,52,653,289,72,717,781,260,181,352,184| )
          ( |130,556,790,910,83,610,915,790,938,202,277,581,738,589,545,85,723,223,338,719| )
          ( |206,676,505,922,486,559,875,373,676,70,726,279,205,156,497,572,573,273,221,900| )
          ( |64,538,888,87,863,78,933,491,979,265,514,316,415,241,324,342,85,557,350,889| )
          ( |534,290,928,735,608,833,280,162,326,305,268,145,569,240,602,829,730,398,68,937| )
          ( |538,740,553,809,704,823,591,216,313,715,980,861,461,558,744,175,868,149,815,333| )
          ( |198,659,735,701,0,734,199,351,493,709,917,439,862,244,208,319,535,859,511,839| )
          ( |253,206,867,312,172,877,723,536,139,324,727,672,471,179,325,67,607,760,936,69| )
          ( |550,235,623,651,512,726,343,211,663,329,82,216,336,518,598,230,489,866,540,149| )
          ( |150,895,636,243,874,876,294,106,789,924,867,237,919,606,347,199,790,63,59,177| )
          ( |345,214,143,339,890,830,446,138,812,543,170,299,863,936,271,283,174,293,601,894| )
          ( |290,208,743,293,660,340,201,213,744,131,558,661,67,653,173,130,425,608,948,588| )
          ( |237,892,339,260,450,584,833,525,302,203,147,565,81,165,393,344,524,107,70,139| )
          ( |755,244,518,110,345,788,287,879,553,51,309,714,609,839,514,154,314,491,144,263| )
          ( |663,68,189,206,245,942,184,192,823,702,79,783,825,858,542,823,673,77,580,903| )
          ( |64,141,193,55,739,654,528,350,898,238,311,199,200,553,321,816,468,592,839,898| )
          ( |935,651,591,53,291,165,905,321,182,786,225,500,185,931,683,474,149,530,820,306| )
          ( |171,812,606,902,593,394,145,541,511,677,896,299,147,328,932,420,257,257,608,826| )
          ( |268,983,128,182,282,606,260,529,668,731,321,568,375,293,667,276,222,286,418,145| )
          ( |248,621,54,709,730,65,415,833,702,154,340,789,458,587,513,744,87,491,207,267| )
          ( |593,913,943,561,285,287,488,439,105,274,170,885,892,343,173,565,561,185,147,88| )
          ( |709,467,110,611,583,150,666,338,264,657,870,562,549,925,182,299,721,185,793,206| )
          ( |588,941,604,910,441,158,299,343,70,555,345,561,125,247,276,57,912,146,152,618| )
          ( |249,919,655,249,525,861,479,81,81,195,243,939,328,498,203,610,314,490,525,72| )
          ( |584,652,3,328,559,391,149,342,348,167,513,392,658,352,375,200,141,487,668,329| )
          ( |284,658,463,417,867,443,243,143,721,331,589,443,190,858,914,245,655,832,267,601| )
          ( |654,663,926,559,831,612,563,722,313,111,676,513,668,604,289,506,675,251,545,875| )
          ( |156,940,565,733,676,704,391,73,58,239,917,285,661,86,240,57,747,871,300,273| )
          ( |572,595,548,143,946,272,656,212,909,572,657,905,558,375,246,816,857,486,753,872| )
          ( |277,129,303,323,227,947,899,881,178,161,658,109,890,281,77,276,946,826,861,941| )
          ( |613,665,866,501,516,225,345,881,981,486,913,557,603,789,710,142,137,143,876,543| )
          ( |949,567,428,167,192,619,393,177,732,680,311,899,147,110,286,659,334,722,597,816| )
          ( |532,652,929,517,196,190,869,544,183,936,535,211,394,918,917,546,915,147,944,621| )
          ( |814,905,306,610,489,234,588,865,812,303,84,869,940,414,415,175,57,474,313,209| )
          ( |337,610,593,56,188,119,582,535,899,79,213,340,917,671,724,536,839,588,681,882| )
          ( |288,224,320,224,612,162,498,242,526,417,408,374,346,174,722,527,729,419,268,676| )
          ( |330,831,520,733,860,575,274,876,183,949,821,154,332,151,889,107,315,905,740,678| )
          ( |572,419,191,78,163,857,876,209,811,339,929,547,60,677,825,277,147,782,344,659| )
          ( |167,921,460,893,299,511,155,892,731,157,293,670,495,218,625,531,79,596,665,192| )
          ( |513,500,277,220,540,424,561,288,673,64,941,882,134,239,172,703,600,916,533,502| )
          ( |738,722,586,655,254,814,170,935,735,745,669,603,240,920,701,187,729,291,840,344| )
          ( |604,490,439,391,590,306,276,788,265,311,784,490,443,327,177,734,726,3,373,83| )
          ( |611,602,266,944,391,514,902,346,232,211,108,718,128,611,263,198,676,894,70,146| )
          ( |293,671,290,814,528,725,705,274,978,624,924,912,152,653,257,510,944,572,268,192| )
          ( |538,164,326,73,810,860,75,816,922,442,129,888,54,855,942,248,159,651,558,226| )
          ( |650,922,332,736,828,328,339,190,744,266,619,921,594,742,867,337,503,173,243,908| )
          ( |589,236,494,743,461,831,839,569,608,413,799,327,70,265,744,50,217,320,834,52| )
          ( |556,908,618,491,339,746,272,880,208,272,556,192,52,786,949,298,519,938,995,717| )
          ( |609,247,559,604,888,349,323,105,895,299,674,716,790,784,283,74,447,156,518,664| )
          ( |855,158,790,720,528,682,335,704,270,322,419,221,533,498,305,516,897,165,371,945| )
          ( |703,882,178,340,933,540,149,533,612,171,157,218,278,106,417,881,681,998,347,197| )
          ( |857,947,602,601,204,201,747,278,908,240,500,348,392,877,51,570,208,150,596,183| )
          ( |718,149,111,521,671,869,537,243,545,668,77,584,622,620,656,825,942,897,450,627| )
          ( |338,949,721,829,244,342,163,941,170,79,569,979,615,352,541,214,896,917,741,442| )
          ( |269,307,52,314,945,719,537,527,662,572,667,607,167,444,865,597,525,247,909,752| )
          ( |121,877,271,270,941,525,374,219,760,915,204,864,223,832,537,746,328,237,77,208| )
          ( |560,190,220,266,375,895,921,171,728,822,471,710,490,420,142,839,556,50,526,559| )
          ( |872,74,878,585,589,854,544,725,859,924,75,536,907,606,618,334,659,682,900,608| )
          ( |501,565,247,740,491,910,892,597,351,917,508,273,545,530,249,720,878,608,546,560| )
          ( |66,879,269,827,65,867,264,836,192,207,513,760,73,288,284,200,349,453,291,811| )
          ( |418,911,817,908,201,264,217,55,153,65,486,497,294,836,128,512,843,136,600,675| )
          ( |207,680,724,513,281,167,934,215,58,596,319,301,136,662,552,856,493,903,86,331| )
          ( |832,415,222,208,198,308,893,6,493,333,68,602,59,736,933,706,730,64,520,298| )
          ( |585,322,511,912,518,758,130,731,622,211,319,658,489,760,216,342,859,916,735,142| )
          ( |920,333,323,239,461,712,733,679,602,575,898,927,84,336,304,84,860,658,513,253| )
          ( |137,192,548,759,746,139,205,832,519,219,894,944,444,177,142,980,659,278,619,161| )
          ( |64,727,106,322,705,72,186,908,250,225,683,826,316,521,79,404,278,109,882,307| )
          ( |264,64,719,835,341,745,444,929,490,56,79,133,419,831,931,554,330,749,944,746| )
          ( |530,827,536,911,275,912,86,707,568,216,582,334,916,512,831,276,376,136,653,932| )
          ( |59,538,865,556,68,209,869,593,81,80,722,157,16,788,170,140,319,556,858,929| )
          ( |948,106,396,311,832,882,600,548,915,674,324,86,324,821,234,925,321,81,523,185| )
          ( |250,239,863,501,902,859,812,949,917,206,880,373,926,729,86,167,119,314,572,499| )
          ( |220,826,655,240,596,543,69,898,553,314,832,521,578,417,882,812,619,347,312,874| )
          ( |511,61,727,623,311,133,790,701,932,550,729,249,342,494,8,154,624,939,106,835| )
          ( |286,206,266,873,899,819,339,730,906,655,734,59,225,518,672,733,734,53,555,797| )
          ( |743,73,598,256,165,410,420,522,306,945,941,931,189,535,908,727,59,110,211,938| )
          ( |935,325,919,721,543,948,278,933,619,940,345,513,870,715,159,986,878,582,660,414| )
          ( |493,172,165,903,948,156,204,326,339,453,334,277,111,618,812,882,261,152,587,416| )
          ( |278,592,319,181,268,211,296,284,818,260,921,834,135,860,608,568,544,115,416,154| )
          ( |202,868,268,930,731,374,898,54,86,141,528,231,567,560,677,657,110,290,184,206| )
          ( |489,590,75,615,79,328,586,62,155,201,167,396,935,162,322,265,152,889,706,375| )
          ( |538,559,246,609,729,303,650,330,521,605,544,821,938,295,625,497,535,63,871,228| )
          ( |920,702,985,315,667,225,551,605,857,308,272,869,534,322,947,441,653,179,319,928| )
          ( |289,416,535,708,928,450,177,166,439,611,289,734,340,600,70,813,321,511,345,947| )
          ( |839,439,620,67,65,443,714,296,345,611,292,461,423,739,59,718,875,297,530,285| )
          ( |938,679,614,595,74,510,933,373,269,374,788,187,643,744,521,829,596,654,254,256| )
          ( |275,512,604,742,915,143,722,650,605,880,156,618,467,83,519,167,411,874,212,610| )
          ( |207,815,732,302,338,712,523,150,714,502,817,671,572,193,275,609,306,653,304,576| )
          ( |393,619,592,261,292,213,709,295,934,586,165,609,496,873,281,320,277,18,322,555| )
          ( |838,495,742,526,185,741,739,393,260,80,911,661,900,762,904,542,183,206,206,944| )
          ( |569,666,67,330,348,347,619,672,838,805,73,444,326,155,900,316,602,731,657,899| )
          ( |666,316,843,896,658,728,623,445,240,624,290,624,834,87,495,591,681,908,904,515| )
          ( |135,496,574,158,132,495,859,588,863,148,243,742,329,824,315,786,659,300,716,160| )
    ).
  ENDMETHOD.
ENDCLASS.