-- 提醒
create table remind (
   id int primary key not null,
   remind_time int not null,
   target_uid int not null,
   target_group_id int not null,
   repeat_times int not null default 3,
   content text not null default '',
   create_uid int not null,
   create_at datetime not null
);

-- 鱼类
create table fish (
   id int primary key not null,
   name varchar(20) not null,
   alias varchar(100)
);

-- 鱼护（篮）
create table moyu_fish_basket (
   id int primary key not null,
   user_id int not null,
   capacity int not null,
   create_at datetime not null
);

-- 鱼护中的鱼
create table moyu_fish_basket_fish (
   basket_id int not null,
   fish_id int not null,
   fish_weight float not null,
   add_at datetime not null
);