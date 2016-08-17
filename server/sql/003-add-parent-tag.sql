alter table tags add column parent_tag integer references tags(id);
