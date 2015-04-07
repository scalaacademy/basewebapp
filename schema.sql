-- table declarations :
create table "sys" (
    "id" bigint primary key not null,
    "db_version" bigint not null
  );
create sequence "s_sys_id";
create table "account" (
    "has_adwords" boolean not null,
    "secret" text not null,
    "account_name" text not null,
    "custom_smtp_config" boolean not null,
    "smtp_config" text not null,
    "id" bigint primary key not null
  );
create sequence "s_account_id";
create table "user" (
    "admin" boolean not null,
    "email" text not null,
    "timezone" text not null,
    "account_id" bigint not null,
    "_salt" text not null,
    "_password" text not null,
    "last_name" text not null,
    "first_name" text not null,
    "id" bigint primary key not null,
    "_locale" text not null,
    "owner" boolean not null,
    "remember_login" text not null,
    "props" text not null,
    "active" boolean not null
  );
create sequence "s_user_id";
-- foreign key constraints :
alter table "user" add constraint "userFK1" foreign key ("account_id") references "account"("id");
