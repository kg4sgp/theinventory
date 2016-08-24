create table tags (
    id integer not null primary key autoincrement
  , name varchar(255) not null
  , creation_dt datetime not null default current_timestamp
  );

create table items (
    id integer primary key autoincrement
  , name varchar(255) not null
  , rating float not null default 0
  , barcode varchar(32)
  , creation_dt datetime not null default current_timestamp
  );
