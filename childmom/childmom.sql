目标：打上母婴标签的用户，未来一个会浏览、搜索母婴用户商品。
数据准备：
注册性别
年龄
用户等级
婚姻状况代码  --1未婚，2已婚，3保密,0未知
近3个月浏览母婴次数 
近3个月搜索母婴次数 
近3个月下单母婴次数
近3个月下单母婴金额  
近3个月是否关注母婴 
近6个月浏览母婴次数 
近6个月搜索母婴次数 
近6个月下单母婴次数 
近6个月下单母婴金额 
近6个月是否关注母婴

衍生变量
是否浏览+搜索母婴 
是否下单母婴


时间周期：近6个月or近3个月。

测试案例暂取3个月

 
code:
create table app_szad_m_dmp_label_childmom_train
ROW FORMAT DELIMITED 
  FIELDS TERMINATED BY '\t' 
STORED AS INPUTFORMAT 
  'com.hadoop.mapred.DeprecatedLzoTextInputFormat' 
OUTPUTFORMAT 
  'org.apache.hadoop.hive.ql.io.HiveIgnoreKeyTextOutputFormat';


set mapred.output.compress=true;
set hive.exec.compress.output=true;
set mapred.output.compression.codec=com.hadoop.compression.lzo.LzopCodec;  
insert overwrite table app.app_szad_m_dmp_label_childmom_train   
select a.user_id,
       case when c.jdpin is not null then 1 else 0 end childmom_tag,  
       member_reg_gender,  --0男,1女,2保密
       nvl(a.age,0) as age,
       a.user_lv_cd,  
       nvl(a.mari_status_cd,0) mari_status_cd,
       nvl(b.pv_nums,0) pv_nums,
       nvl(b.search_nums,0) search_nums,
       nvl(b.gen_nums,0) gen_nums,
       nvl(b.gen_fee,0) gen_fee,
       nvl(b.favorite_tag,0) favorite_tag
  from (select user_id,user_log_acct,user_lv_cd,member_reg_gender,mari_status_cd,
               last_create_ord_tm,
               2016-cast(substr(reg_birthday,1,4) as bigint) age 
          from gdm.gdm_m01_userinfo_basic_sum
         where dt='2016-07-31' 
           and (reg_user_type_cd<100 or reg_user_type_cd is null)
           and blacklist_state is null
           and blacklist_type is null  
           and last_create_ord_tm >='2016-03-01'  --最近一段时间有下单的
           and user_lv_cd in('56','61','62','105')  --50,注册会员,56,铜牌,61,银牌,62金牌,105,钻石
           and length(user_log_acct)>5
       ) a 
  left join (select  jdpin
                ,sum(pv_nums) as pv_nums
                ,sum(search_nums)  search_nums 
                ,sum(gen_nums)  gen_nums 
                ,sum(gen_fee)  gen_fee  
                ,max(case when fav_nums>0 then 1 else 0 end)  favorite_tag
          from app.app_szad_m_dmp_aggrateby_jdpin_cate_day 
        where dt>='2016-04-01' and dt<= '2016-06-30' 
          and length(jdpin)>5 
          and item_first_cate_cd='1319'
          group by jdpin
       ) b 
    on a.user_log_acct=b.jdpin
  left join(select jdpin
         from app.app_szad_m_dmp_mkt_day
        where dt>='2016-07-01' and dt<='2016-07-31'
          and item_first_cate_cd='1319'
          and length(jdpin)>5
        group by jdpin ) c
    on a.user_log_acct=c.jdpin   

 
测试样本 1%
hive -e "select * from app.app_szad_m_dmp_label_childmom_train TABLESAMPLE (BUCKET 1 OUT OF 100 ON rand());">childmom.txt