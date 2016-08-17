create table tags_items (
    id integer primary key autoincrement
  , tag_id integer not null
  , item_id integer not null
  , foreign key (tag_id) references tags(id)
  , foreign key (item_id) references item(id)
  );
