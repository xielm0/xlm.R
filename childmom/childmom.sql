Ŀ�꣺����ĸӤ��ǩ���û���δ��һ�������������ĸӤ�û���Ʒ��
����׼����
ע���Ա�
����
�û��ȼ�
����״������  --1δ�飬2�ѻ飬3����,0δ֪
��3�������ĸӤ���� 
��3��������ĸӤ���� 
��3�����µ�ĸӤ����
��3�����µ�ĸӤ���  
��3�����Ƿ��עĸӤ 
��6�������ĸӤ���� 
��6��������ĸӤ���� 
��6�����µ�ĸӤ���� 
��6�����µ�ĸӤ��� 
��6�����Ƿ��עĸӤ

��������
�Ƿ����+����ĸӤ 
�Ƿ��µ�ĸӤ


ʱ�����ڣ���6����or��3���¡�

���԰�����ȡ3����

 
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
       member_reg_gender,  --0��,1Ů,2����
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
           and last_create_ord_tm >='2016-03-01'  --���һ��ʱ�����µ���
           and user_lv_cd in('56','61','62','105')  --50,ע���Ա,56,ͭ��,61,����,62����,105,��ʯ
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

 
�������� 1%
hive -e "select * from app.app_szad_m_dmp_label_childmom_train TABLESAMPLE (BUCKET 1 OUT OF 100 ON rand());">childmom.txt