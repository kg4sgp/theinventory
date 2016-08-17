create table tags (
    id integer not null primary key autoincrement
  , name varchar(255) not null
  );

create table items (
    id integer primary key autoincrement
  , name varchar(255) not null
  , rating float not null default 0
  , barcode varchar(32)
  );
