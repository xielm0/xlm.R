xxƷ������
hive -e " select jd_price  from app.app_szad_sku_prive_lev_day_t1 where item_third_cate_cd=655 " >price1.txt 


xxƷ���µ������ݡ�  
hive -e " select jd_price,count(1) 
  from (SELECT   
                item_third_cate_cd     ,
                case when after_prefr_unit_price<10000 then bigint(concat(floor(after_prefr_unit_price/1000),'900'))   -- ��Ϊ3500��3900
                     when after_prefr_unit_price>=10000 then bigint(concat(floor(after_prefr_unit_price/10000),'9900'))  -- ��35500��39900
                end as jd_price 
          FROM  app.app_szad_m_dmp_user_basic_info_day
         where dt>'2016-08-01'  and dt<='2016-08-07'
           and action_type=5
           and item_third_cate_cd=655
           and after_prefr_unit_price>0
         )t  
 group by jd_price" >price2.txt 