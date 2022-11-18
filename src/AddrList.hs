module AddrList where

import Data.Text ( Text, pack )

addrref :: [[Text]]
addrref = map (map pack) [
    ["09007010","連江縣南竿"],
    ["09007020","連江縣北竿"],
    ["09007030","連江縣莒光"],
    ["09007040","連江縣東引"],
    ["09020010","金門縣金城"],
    ["09020020","金門縣金沙"],
    ["09020030","金門縣金湖"],
    ["09020040","金門縣金寧"],
    ["09020050","金門縣烈嶼"],
    ["09020060","金門縣烏坵"],
    ["10002010","宜蘭縣宜蘭"],
    ["10002010","宜蘭市"],
    ["10002020","宜蘭縣羅東"],
    ["10002030","宜蘭縣蘇澳"],
    ["10002040","宜蘭縣頭城"],
    ["10002050","宜蘭縣礁溪"],
    ["10002060","宜蘭縣壯圍"],
    ["10002070","宜蘭縣員山"],
    ["10002080","宜蘭縣冬山"],
    ["10002090","宜蘭縣五結"],
    ["10002100","宜蘭縣三星"],
    ["10002110","宜蘭縣大同"],
    ["10002120","宜蘭縣南澳"],
    ["10004010","新竹縣竹北"],
    ["10004020","新竹縣竹東"],
    ["10004030","新竹縣新埔"],
    ["10004040","新竹縣關西"],
    ["10004050","新竹縣湖口"],
    ["10004060","新竹縣新豐"],
    ["10004070","新竹縣芎林"],
    ["10004080","新竹縣橫山"],
    ["10004090","新竹縣北埔"],
    ["10004100","新竹縣寶山"],
    ["10004110","新竹縣峨眉"],
    ["10004120","新竹縣尖石"],
    ["10004130","新竹縣五峰"],
    ["10005010","苗栗縣苗栗"],
    ["10005020","苗栗縣苑裡"],
    ["10005030","苗栗縣通霄"],
    ["10005040","苗栗縣竹南"],
    ["10005050","苗栗縣頭份"],
    ["10005060","苗栗縣後龍"],
    ["10005070","苗栗縣卓蘭"],
    ["10005080","苗栗縣大湖"],
    ["10005090","苗栗縣公館"],
    ["10005100","苗栗縣銅鑼"],
    ["10005110","苗栗縣南庄"],
    ["10005120","苗栗縣頭屋"],
    ["10005130","苗栗縣三義"],
    ["10005140","苗栗縣西湖"],
    ["10005150","苗栗縣造橋"],
    ["10005160","苗栗縣三灣"],
    ["10005170","苗栗縣獅潭"],
    ["10005180","苗栗縣泰安"],
    ["10007010","彰化縣彰化"],
    ["10007020","彰化縣鹿港"],
    ["10007030","彰化縣和美"],
    ["10007040","彰化縣線西"],
    ["10007050","彰化縣伸港"],
    ["10007060","彰化縣福興"],
    ["10007070","彰化縣秀水"],
    ["10007080","彰化縣花壇"],
    ["10007090","彰化縣芬園"],
    ["10007100","彰化縣員林"],
    ["10007110","彰化縣溪湖"],
    ["10007120","彰化縣田中"],
    ["10007130","彰化縣大村"],
    ["10007140","彰化縣埔鹽"],
    ["10007150","彰化縣埔心"],
    ["10007160","彰化縣永靖"],
    ["10007170","彰化縣社頭"],
    ["10007180","彰化縣二水"],
    ["10007190","彰化縣北斗"],
    ["10007200","彰化縣二林"],
    ["10007210","彰化縣田尾"],
    ["10007220","彰化縣埤頭"],
    ["10007230","彰化縣芳苑"],
    ["10007240","彰化縣大城"],
    ["10007250","彰化縣竹塘"],
    ["10007260","彰化縣溪州"],
    ["10008010","南投縣南投"],
    ["10008020","南投縣埔里"],
    ["10008030","南投縣草屯"],
    ["10008040","南投縣竹山"],
    ["10008050","南投縣集集"],
    ["10008060","南投縣名間"],
    ["10008070","南投縣鹿谷"],
    ["10008080","南投縣中寮"],
    ["10008090","南投縣魚池"],
    ["10008100","南投縣國姓"],
    ["10008110","南投縣水里"],
    ["10008120","南投縣信義"],
    ["10008130","南投縣仁愛"],
    ["10009010","雲林縣斗六"],
    ["10009020","雲林縣斗南"],
    ["10009030","雲林縣虎尾"],
    ["10009040","雲林縣西螺"],
    ["10009050","雲林縣土庫"],
    ["10009060","雲林縣北港"],
    ["10009070","雲林縣古坑"],
    ["10009080","雲林縣大埤"],
    ["10009090","雲林縣莿桐"],
    ["10009100","雲林縣林內"],
    ["10009110","雲林縣二崙"],
    ["10009120","雲林縣崙背"],
    ["10009130","雲林縣麥寮"],
    ["10009140","雲林縣東勢"],
    ["10009150","雲林縣褒忠"],
    ["10009160","雲林縣台西"],
    ["10009160","雲林縣臺西"],
    ["10009170","雲林縣元長"],
    ["10009180","雲林縣四湖"],
    ["10009190","雲林縣口湖"],
    ["10009200","雲林縣水林"],
    ["10010010","嘉義縣太保"],
    ["10010020","嘉義縣朴子"],
    ["10010030","嘉義縣布袋"],
    ["10010040","嘉義縣大林"],
    ["10010050","嘉義縣民雄"],
    ["10010060","嘉義縣溪口"],
    ["10010070","嘉義縣新港"],
    ["10010080","嘉義縣六腳"],
    ["10010090","嘉義縣東石"],
    ["10010100","嘉義縣義竹"],
    ["10010110","嘉義縣鹿草"],
    ["10010120","嘉義縣水上"],
    ["10010130","嘉義縣中埔"],
    ["10010140","嘉義縣竹崎"],
    ["10010150","嘉義縣梅山"],
    ["10010160","嘉義縣番路"],
    ["10010170","嘉義縣大埔"],
    ["10010180","嘉義縣阿里山"],
    ["10013010","屏東縣屏東"],
    ["10013020","屏東縣潮州"],
    ["10013030","屏東縣東港"],
    ["10013040","屏東縣恆春"],
    ["10013050","屏東縣萬丹"],
    ["10013060","屏東縣長治"],
    ["10013070","屏東縣麟洛"],
    ["10013080","屏東縣九如"],
    ["10013090","屏東縣里港"],
    ["10013100","屏東縣鹽埔"],
    ["10013110","屏東縣高樹"],
    ["10013120","屏東縣萬巒"],
    ["10013130","屏東縣內埔"],
    ["10013140","屏東縣竹田"],
    ["10013150","屏東縣新埤"],
    ["10013160","屏東縣枋寮"],
    ["10013170","屏東縣新園"],
    ["10013180","屏東縣崁頂"],
    ["10013190","屏東縣林邊"],
    ["10013200","屏東縣南州"],
    ["10013210","屏東縣佳冬"],
    ["10013220","屏東縣琉球"],
    ["10013230","屏東縣車城"],
    ["10013240","屏東縣滿州"],
    ["10013250","屏東縣枋山"],
    ["10013260","屏東縣三地門"],
    ["10013270","屏東縣霧臺"],
    ["10013270","屏東縣霧台"],
    ["10013280","屏東縣瑪家"],
    ["10013290","屏東縣泰武"],
    ["10013300","屏東縣來義"],
    ["10013310","屏東縣春日"],
    ["10013320","屏東縣獅子"],
    ["10013330","屏東縣牡丹"],
    ["10014010","東縣台東"],
    ["10014010","東縣臺東"],
    ["10014020","東縣成功"],
    ["10014030","東縣關山"],
    ["10014040","東縣卑南"],
    ["10014050","東縣鹿野"],
    ["10014060","東縣池上"],
    ["10014070","東縣東河"],
    ["10014080","東縣長濱"],
    ["10014090","東縣太麻里"],
    ["10014100","東縣大武"],
    ["10014110","東縣綠島"],
    ["10014120","東縣海端"],
    ["10014130","東縣延平"],
    ["10014140","東縣金峰"],
    ["10014150","東縣達仁"],
    ["10014160","東縣蘭嶼"],
    ["10015010","花蓮縣花蓮"],
    ["10015020","花蓮縣鳳林"],
    ["10015030","花蓮縣玉里"],
    ["10015040","花蓮縣新城"],
    ["10015050","花蓮縣吉安"],
    ["10015060","花蓮縣壽豐"],
    ["10015070","花蓮縣光復"],
    ["10015080","花蓮縣豐濱"],
    ["10015090","花蓮縣瑞穗"],
    ["10015100","花蓮縣富里"],
    ["10015110","花蓮縣秀林"],
    ["10015120","花蓮縣萬榮"],
    ["10015130","花蓮縣卓溪"],
    ["10016010","澎湖縣馬公"],
    ["10016020","澎湖縣湖西"],
    ["10016030","澎湖縣白沙"],
    ["10016040","澎湖縣西嶼"],
    ["10016050","澎湖縣望安"],
    ["10016060","澎湖縣七美"],
    ["10017010","基隆市中正"],
    ["10017020","基隆市七堵"],
    ["10017030","基隆市暖暖"],
    ["10017040","基隆市仁愛"],
    ["10017050","基隆市中山"],
    ["10017060","基隆市安樂"],
    ["10017070","基隆市信義"],

    ["10017010","基隆巿中正"],
    ["10017020","基隆巿七堵"],
    ["10017030","基隆巿暖暖"],
    ["10017040","基隆巿仁愛"],
    ["10017050","基隆巿中山"],
    ["10017060","基隆巿安樂"],
    ["10017070","基隆巿信義"],

    ["10018010","新竹市東"],
    ["10018020","新竹市北"],
    ["10018030","新竹市香山"],
    ["10020010","嘉義市東"],
    ["10020020","嘉義市西"],

    ["10018010","新竹巿東"],
    ["10018020","新竹巿北"],
    ["10018030","新竹巿香山"],
    ["10020010","嘉義巿東"],
    ["10020020","嘉義巿西"],

    ["63000010","松山區"],
    ["63000020","北市信義"],
    ["63000030","北市大安"],
    ["63000040","北市中山"],
    ["63000050","北市中正"],

    ["63000020","北巿信義"],
    ["63000030","北巿大安"],
    ["63000040","北巿中山"],
    ["63000050","北巿中正"],

    ["63000060","大同區"],
    ["63000070","萬華"],
    ["63000080","文山區"],
    ["63000090","南港"],
    ["63000100","內湖"],
    ["63000110","士林"],
    ["63000120","北投"],
    ["63000120","石牌"],
    ["64000010","高雄市鹽埕"],
    ["64000020","高雄市鼓山"],
    ["64000030","高雄市左營"],
    ["64000040","高雄市楠梓"],
    ["64000050","高雄市三民"],
    ["64000060","高雄市新興"],
    ["64000070","高雄市前金"],
    ["64000080","高雄市苓雅"],
    ["64000090","高雄市前鎮"],
    ["64000100","高雄市旗津"],
    ["64000110","高雄市小港"],
    ["64000120","高雄市鳳山"],
    ["64000130","高雄市林園"],
    ["64000140","高雄市大寮"],
    ["64000150","高雄市大樹"],
    ["64000160","高雄市大社"],
    ["64000170","高雄市仁武"],
    ["64000180","高雄市鳥松"],
    ["64000190","高雄市岡山"],
    ["64000200","高雄市橋頭"],
    ["64000210","高雄市燕巢"],
    ["64000220","高雄市田寮"],
    ["64000230","高雄市阿蓮"],
    ["64000240","高雄市路竹"],
    ["64000250","高雄市湖內"],
    ["64000260","高雄市茄萣"],
    ["64000270","高雄市永安"],
    ["64000280","高雄市彌陀"],
    ["64000290","高雄市梓官"],
    ["64000300","高雄市旗山"],
    ["64000310","高雄市美濃"],
    ["64000320","高雄市六龜"],
    ["64000330","高雄市甲仙"],
    ["64000340","高雄市杉林"],
    ["64000350","高雄市內門"],
    ["64000360","高雄市茂林"],
    ["64000370","高雄市桃源"],
    ["64000380","高雄市那瑪夏"],

    ["64000010","高雄巿鹽埕"],
    ["64000020","高雄巿鼓山"],
    ["64000030","高雄巿左營"],
    ["64000040","高雄巿楠梓"],
    ["64000050","高雄巿三民"],
    ["64000060","高雄巿新興"],
    ["64000070","高雄巿前金"],
    ["64000080","高雄巿苓雅"],
    ["64000090","高雄巿前鎮"],
    ["64000100","高雄巿旗津"],
    ["64000110","高雄巿小港"],
    ["64000120","高雄巿鳳山"],
    ["64000130","高雄巿林園"],
    ["64000140","高雄巿大寮"],
    ["64000150","高雄巿大樹"],
    ["64000160","高雄巿大社"],
    ["64000170","高雄巿仁武"],
    ["64000180","高雄巿鳥松"],
    ["64000190","高雄巿岡山"],
    ["64000200","高雄巿橋頭"],
    ["64000210","高雄巿燕巢"],
    ["64000220","高雄巿田寮"],
    ["64000230","高雄巿阿蓮"],
    ["64000240","高雄巿路竹"],
    ["64000250","高雄巿湖內"],
    ["64000260","高雄巿茄萣"],
    ["64000270","高雄巿永安"],
    ["64000280","高雄巿彌陀"],
    ["64000290","高雄巿梓官"],
    ["64000300","高雄巿旗山"],
    ["64000310","高雄巿美濃"],
    ["64000320","高雄巿六龜"],
    ["64000330","高雄巿甲仙"],
    ["64000340","高雄巿杉林"],
    ["64000350","高雄巿內門"],
    ["64000360","高雄巿茂林"],
    ["64000370","高雄巿桃源"],
    ["64000380","高雄巿那瑪夏"],

    ["64000120","高雄縣鳳山"],
    ["64000130","高雄縣林園"],
    ["64000140","高雄縣大寮"],
    ["64000150","高雄縣大樹"],
    ["64000160","高雄縣大社"],
    ["64000170","高雄縣仁武"],
    ["64000180","高雄縣鳥松"],
    ["64000190","高雄縣岡山"],
    ["64000200","高雄縣橋頭"],
    ["64000210","高雄縣燕巢"],
    ["64000220","高雄縣田寮"],
    ["64000230","高雄縣阿蓮"],
    ["64000240","高雄縣路竹"],
    ["64000250","高雄縣湖內"],
    ["64000260","高雄縣茄萣"],
    ["64000270","高雄縣永安"],
    ["64000280","高雄縣彌陀"],
    ["64000290","高雄縣梓官"],
    ["64000300","高雄縣旗山"],
    ["64000310","高雄縣美濃"],
    ["64000320","高雄縣六龜"],
    ["64000330","高雄縣甲仙"],
    ["64000340","高雄縣杉林"],
    ["64000350","高雄縣內門"],
    ["64000360","高雄縣茂林"],
    ["64000370","高雄縣桃源"],
    ["64000380","高雄縣那瑪夏"],
    ["65000010","板橋"],
    ["65000020","三重"],
    ["65000030","中和"],
    ["65000040","永和"],
    ["65000050","新莊"],
    ["65000060","新店"],
    ["65000070","樹林區"],
    ["65000070","北縣樹林"],
    ["65000080","鶯歌"],
    ["65000090","三峽"],
    ["65000100","淡水"],
    ["65000110","汐止"],
    ["65000120","瑞芳"],
    ["65000130","土城"],
    ["65000140","蘆洲"],
    ["65000150","五股"],
    ["65000160","泰山"],
    ["65000170","林口"],
    ["65000180","深坑"],
    ["65000190","石碇"],
    ["65000200","坪林區"],
    ["65000200","北縣坪林鄉"],
    ["65000210","三芝"],
    ["65000220","石門"],
    ["65000230","八里"],
    ["65000240","平溪"],
    ["65000250","雙溪"],
    ["65000260","貢寮"],
    ["65000270","金山"],
    ["65000280","萬里"],
    ["65000290","烏來"],
    ["66000010","中市中"],
    ["66000020","中市東"],
    ["66000030","中市南"],
    ["66000040","中市西"],
    ["66000050","中市北"],
    ["66000060","中市西屯"],
    ["66000070","中市南屯"],
    ["66000080","中市北屯"],
    ["66000090","中市豐原"],
    ["66000100","中市東勢"],
    ["66000110","中市大甲"],
    ["66000120","中市清水"],
    ["66000130","中市沙鹿"],
    ["66000140","中市梧棲"],
    ["66000150","中市后里"],
    ["66000160","中市神岡"],
    ["66000170","中市潭子"],
    ["66000180","中市大雅"],
    ["66000190","中市新社"],
    ["66000200","中市石岡"],
    ["66000210","中市外埔"],
    ["66000220","中市大安"],
    ["66000230","中市烏日"],
    ["66000240","中市大肚"],
    ["66000250","中市龍井"],
    ["66000260","中市霧峰"],
    ["66000270","中市太平"],
    ["66000280","中市大里"],
    ["66000290","中市和平"],

    ["66000010","中巿中"],
    ["66000020","中巿東"],
    ["66000030","中巿南"],
    ["66000040","中巿西"],
    ["66000050","中巿北"],
    ["66000060","中巿西屯"],
    ["66000070","中巿南屯"],
    ["66000080","中巿北屯"],
    ["66000090","中巿豐原"],
    ["66000100","中巿東勢"],
    ["66000110","中巿大甲"],
    ["66000120","中巿清水"],
    ["66000130","中巿沙鹿"],
    ["66000140","中巿梧棲"],
    ["66000150","中巿后里"],
    ["66000160","中巿神岡"],
    ["66000170","中巿潭子"],
    ["66000180","中巿大雅"],
    ["66000190","中巿新社"],
    ["66000200","中巿石岡"],
    ["66000210","中巿外埔"],
    ["66000220","中巿大安"],
    ["66000230","中巿烏日"],
    ["66000240","中巿大肚"],
    ["66000250","中巿龍井"],
    ["66000260","中巿霧峰"],
    ["66000270","中巿太平"],
    ["66000280","中巿大里"],
    ["66000290","中巿和平"],

    ["66000090","中縣豐原"],
    ["66000100","中縣東勢"],
    ["66000110","中縣大甲"],
    ["66000120","中縣清水"],
    ["66000130","中縣沙鹿"],
    ["66000140","中縣梧棲"],
    ["66000150","中縣后里"],
    ["66000160","中縣神岡"],
    ["66000170","中縣潭子"],
    ["66000180","中縣大雅"],
    ["66000190","中縣新社"],
    ["66000200","中縣石岡"],
    ["66000210","中縣外埔"],
    ["66000220","中縣大安"],
    ["66000230","中縣烏日"],
    ["66000240","中縣大肚"],
    ["66000250","中縣龍井"],
    ["66000260","中縣霧峰"],
    ["66000270","中縣太平"],
    ["66000280","中縣大里"],
    ["66000290","中縣和平"],
    ["67000010","南市新營"],
    ["67000020","南市鹽水"],
    ["67000030","南市白河"],
    ["67000040","南市柳營"],
    ["67000050","南市後壁"],
    ["67000060","南市東山"],
    ["67000070","南市麻豆"],
    ["67000080","南市下營"],
    ["67000090","南市六甲"],
    ["67000100","南市官田"],
    ["67000110","南市大內"],
    ["67000120","南市佳里"],
    ["67000130","南市學甲"],
    ["67000140","南市西港"],
    ["67000150","南市七股"],
    ["67000160","南市將軍"],
    ["67000170","南市北門"],
    ["67000180","南市新化"],
    ["67000190","南市善化"],
    ["67000200","南市新市"],
    ["67000210","南市安定"],
    ["67000220","南市山上"],
    ["67000230","南市玉井"],
    ["67000240","南市楠西"],
    ["67000250","南市南化"],
    ["67000260","南市左鎮"],
    ["67000270","南市仁德"],
    ["67000280","南市歸仁"],
    ["67000290","南市關廟"],
    ["67000300","南市龍崎"],
    ["67000310","南市永康"],

    ["67000010","南巿新營"],
    ["67000020","南巿鹽水"],
    ["67000030","南巿白河"],
    ["67000040","南巿柳營"],
    ["67000050","南巿後壁"],
    ["67000060","南巿東山"],
    ["67000070","南巿麻豆"],
    ["67000080","南巿下營"],
    ["67000090","南巿六甲"],
    ["67000100","南巿官田"],
    ["67000110","南巿大內"],
    ["67000120","南巿佳里"],
    ["67000130","南巿學甲"],
    ["67000140","南巿西港"],
    ["67000150","南巿七股"],
    ["67000160","南巿將軍"],
    ["67000170","南巿北門"],
    ["67000180","南巿新化"],
    ["67000190","南巿善化"],
    ["67000200","南巿新巿"],
    ["67000210","南巿安定"],
    ["67000220","南巿山上"],
    ["67000230","南巿玉井"],
    ["67000240","南巿楠西"],
    ["67000250","南巿南化"],
    ["67000260","南巿左鎮"],
    ["67000270","南巿仁德"],
    ["67000280","南巿歸仁"],
    ["67000290","南巿關廟"],
    ["67000300","南巿龍崎"],
    ["67000310","南巿永康"],

    ["67000010","南縣新營"],
    ["67000020","南縣鹽水"],
    ["67000030","南縣白河"],
    ["67000040","南縣柳營"],
    ["67000050","南縣後壁"],
    ["67000060","南縣東山"],
    ["67000070","南縣麻豆"],
    ["67000080","南縣下營"],
    ["67000090","南縣六甲"],
    ["67000100","南縣官田"],
    ["67000110","南縣大內"],
    ["67000120","南縣佳里"],
    ["67000130","南縣學甲"],
    ["67000140","南縣西港"],
    ["67000150","南縣七股"],
    ["67000160","南縣將軍"],
    ["67000170","南縣北門"],
    ["67000180","南縣新化"],
    ["67000190","南縣善化"],
    ["67000200","南縣新縣"],
    ["67000210","南縣安定"],
    ["67000220","南縣山上"],
    ["67000230","南縣玉井"],
    ["67000240","南縣楠西"],
    ["67000250","南縣南化"],
    ["67000260","南縣左鎮"],
    ["67000270","南縣仁德"],
    ["67000280","南縣歸仁"],
    ["67000290","南縣關廟"],
    ["67000300","南縣龍崎"],
    ["67000310","南縣永康"],
    ["67000320","南市東"],
    ["67000330","南市南"],
    ["67000340","南市北"],
    ["67000350","南市安南"],
    ["67000360","南市安平"],
    ["67000370","南市中西"],
    ["68000020","中壢"],
    ["68000040","楊梅"],
    ["68000010","桃園市桃園"],
    ["68000030","桃園市大溪"],
    ["68000050","桃園市蘆竹"],
    ["68000060","桃園市大園"],
    ["68000070","桃園市龜山"],
    ["68000080","桃園市八德"],
    ["68000090","桃園市龍潭"],
    ["68000100","桃園市平鎮"],
    ["68000110","桃園市新屋"],
    ["68000120","桃園市觀音"],
    ["68000130","桃園市復興"],

    ["68000010","桃園巿桃園"],
    ["68000030","桃園巿大溪"],
    ["68000050","桃園巿蘆竹"],
    ["68000060","桃園巿大園"],
    ["68000070","桃園巿龜山"],
    ["68000080","桃園巿八德"],
    ["68000090","桃園巿龍潭"],
    ["68000100","桃園巿平鎮"],
    ["68000110","桃園巿新屋"],
    ["68000120","桃園巿觀音"],
    ["68000130","桃園巿復興"],

    ["68000010","桃園縣桃園"],
    ["68000020","中壢"],
    ["68000030","桃園縣大溪"],
    ["68000040","楊梅"],
    ["68000050","桃園縣蘆竹"],
    ["68000060","桃園縣大園"],
    ["68000070","桃園縣龜山"],
    ["68000080","桃園縣八德"],
    ["68000090","桃園縣龍潭"],
    ["68000100","桃園縣平鎮"],
    ["68000110","桃園縣新屋"],
    ["68000120","桃園縣觀音"],
    ["68000130","桃園縣復興"]]