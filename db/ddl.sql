-- 提醒
create table remind (
   id integer primary key autoincrement not null,
   remind_time int not null, -- 提醒时间
   target_uid int not null, -- 提醒用户id，为0表示无特定用户
   target_group_id int not null, -- 提醒消息发送的群组，为0则表示私发
   repeat_times int not null default 3, -- 响铃重复次数
   content text not null default '', -- 内容
   create_uid int not null, -- 创建用户id
   create_at datetime not null  -- 创建时间
);