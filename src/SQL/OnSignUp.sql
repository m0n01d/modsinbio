DROP TRIGGER IF EXISTS trigger_categories_creation on "users";
DROP FUNCTION IF EXISTS trigger_on_sign_up();
CREATE FUNCTION trigger_on_sign_up()
  RETURNS trigger AS $BODY$
DECLARE user_id INT;
BEGIN
  user_id := New.id;
  INSERT INTO categories (name, "order", owner) VALUES
  ('Engine', 0, user_id),
  ('Exterior', 1, user_id),
  ('Interior', 2, user_id),
  ('Chassis', 3, user_id),
  ('Wheels', 4, user_id),
  ('Accessories', 5, user_id),
  ('Misc', 6, user_id);
  
RETURN NEW;
END;
$BODY$ LANGUAGE plpgsql;



CREATE TRIGGER trigger_categories_creation
  AFTER INSERT ON "users"
  FOR EACH ROW EXECUTE PROCEDURE trigger_on_sign_up();