CREATE FUNCTION public.dup(integer, OUT f1 integer, OUT f2 text) RETURNS record
    LANGUAGE sql
    AS $_$ SELECT $1, CAST($1 AS text) || ' is text' $_$;
CREATE FUNCTION public.trigger_on_sign_up() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE user_id uuid;
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
$$;
CREATE TABLE public.categories (
    name text NOT NULL,
    id uuid DEFAULT public.gen_random_uuid() NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    "order" integer NOT NULL,
    owner uuid NOT NULL,
    soft_delete boolean DEFAULT false NOT NULL
);
CREATE TABLE public.links (
    "urlString" text NOT NULL,
    protocol text NOT NULL,
    host text NOT NULL,
    path text NOT NULL,
    query text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    fragment text,
    title text NOT NULL,
    active boolean DEFAULT true NOT NULL,
    description text,
    category_id uuid NOT NULL,
    soft_delete boolean DEFAULT false NOT NULL,
    id uuid DEFAULT public.gen_random_uuid() NOT NULL
);
CREATE TABLE public.link_clicks (
    link uuid NOT NULL,
    id uuid DEFAULT public.gen_random_uuid() NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);
CREATE TABLE public.role (
    name text NOT NULL,
    id uuid DEFAULT public.gen_random_uuid() NOT NULL
);
CREATE TABLE public.user_role (
    user_id uuid NOT NULL,
    role_id uuid NOT NULL,
    id uuid DEFAULT public.gen_random_uuid() NOT NULL
);
CREATE TABLE public.users (
    id uuid DEFAULT public.gen_random_uuid() NOT NULL,
    username text,
    created timestamp with time zone DEFAULT now() NOT NULL,
    profile jsonb,
    views integer DEFAULT 0 NOT NULL,
    email text NOT NULL
);
ALTER TABLE ONLY public.categories
    ADD CONSTRAINT categories_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.link_clicks
    ADD CONSTRAINT link_clicks_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.links
    ADD CONSTRAINT links_id_key UNIQUE (id);
ALTER TABLE ONLY public.links
    ADD CONSTRAINT links_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.role
    ADD CONSTRAINT role_id_key UNIQUE (id);
ALTER TABLE ONLY public.role
    ADD CONSTRAINT role_name_key UNIQUE (name);
ALTER TABLE ONLY public.role
    ADD CONSTRAINT role_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.user_role
    ADD CONSTRAINT user_role_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_email_key UNIQUE (email);
ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_pkey1 PRIMARY KEY (id);
ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_username_key1 UNIQUE (username);
CREATE TRIGGER trigger_categories_creation AFTER INSERT ON public.users FOR EACH ROW EXECUTE FUNCTION public.trigger_on_sign_up();
ALTER TABLE ONLY public.categories
    ADD CONSTRAINT categories_owner_fkey FOREIGN KEY (owner) REFERENCES public.users(id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.link_clicks
    ADD CONSTRAINT link_clicks_link_fkey FOREIGN KEY (link) REFERENCES public.links(id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.links
    ADD CONSTRAINT links_category_id_fkey FOREIGN KEY (category_id) REFERENCES public.categories(id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.user_role
    ADD CONSTRAINT user_role_role_id_fkey FOREIGN KEY (role_id) REFERENCES public.role(id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.user_role
    ADD CONSTRAINT user_role_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON UPDATE RESTRICT ON DELETE RESTRICT;
