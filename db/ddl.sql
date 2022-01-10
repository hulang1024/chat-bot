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