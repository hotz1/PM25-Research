-- CREATE EXTENSION postgis;
SELECT *
FROM geometry_columns;



-- SELECT pixels.pixel_id
-- FROM pixels, iran
-- WHERE st_intersects(pixels.geom, iran.geom);
--
-- SELECT pixels.pixel_id
-- FROM pixels, iran_large
-- WHERE st_intersects(pixels.geom, st_envelope(iran_large.geom));
-- SELECT pixels.pixel_id
-- FROM pixels, iran_large
-- WHERE st_intersects(pixels.geom, iran_large.geom);
--
-- SELECT pixels.pixel_id
-- FROM pixels, iran_large
-- WHERE st_intersects(st_extent(iran_large.geom), pixels.geom);

SELECT count(pixel_id) from pixels;

SELECT pixel_id, st_x(geom) AS lon, st_y(geom) AS lat
FROM pixels
WHERE substr(pixel_id, 1,4) = 'P040';


SELECT p.lon, p.lat, dat.*
FROM
  (SELECT pixels.pixel_id, st_x(pixels.geom) AS lon, st_y(pixels.geom) AS lat
   FROM pixels, kuwait
   WHERE st_intersects(kuwait.geom, pixels.geom)) AS p
  JOIN
    (SELECT * FROM misr_prods
                   JOIN misr_aux_raw USING (pixel_id, date_time)) AS dat USING (pixel_id);

SELECT * FROM pixels;
select * from misr_prods;

SELECT pg_size_pretty(pg_database_size('misr'));
-- SELECT round(pg_database_size('misr') / 1024 ^ 3 :: NUMERIC, 3) AS size_GB;

SELECT pg_size_pretty(pg_total_relation_size('iran'));

-- CREATE MATERIALIZED VIEW iran_subdiv AS
-- SELECT iso3 AS country, st_subdivide(geom) as geom
-- FROM iran;


--/* PERFORMANCE: MATERIALIZED VIEWS
drop MATERIALIZED VIEW kuwait_data;

-- CREATE MATERIALIZED VIEW kuwait_pixels AS
--   SELECT pixel_id, st_x(pixels.geom) AS lon, st_y(pixels.geom) AS lat
--   FROM pixels, kuwait
--   WHERE st_intersects(kuwait.geom, pixels.geom);
CREATE MATERIALIZED VIEW cali_pixels AS
  SELECT pixel_id, st_x(pixels.geom) AS lon, st_y(pixels.geom) AS lat
  FROM pixels, cali
  WHERE st_intersects(cali.geom, pixels.geom);

CREATE MATERIALIZED VIEW kuwait_data AS
  SELECT p.lon, p.lat, dat.*
  FROM
    (SELECT pixels.pixel_id, st_x(pixels.geom) AS lon, st_y(pixels.geom) AS lat
     FROM pixels, kuwait
     WHERE st_intersects(kuwait.geom, pixels.geom)) AS p
    JOIN
      (SELECT * FROM misr_prods
       JOIN misr_aux_raw USING (pixel_id, date_time)) AS dat USING (pixel_id);

CREATE MATERIALIZED VIEW cali_data AS
  SELECT p.lon, p.lat, dat.*
  FROM
    (SELECT pixels.pixel_id, st_x(pixels.geom) AS lon, st_y(pixels.geom) AS lat
     FROM pixels, cali
     WHERE st_intersects(cali.geom, pixels.geom)) AS p
    JOIN
      (SELECT * FROM misr_prods
       JOIN misr_aux_raw USING (pixel_id, date_time)) AS dat USING (pixel_id);

CREATE MATERIALIZED VIEW stn_pix AS
  SELECT stn.cbsa_name, p.pixel_id, st_x(p.geom) AS lon, st_y(p.geom) AS lat,
         st_distance(st_transform(stn.geom, 26911),
                     st_transform(p.geom, 26911)) AS dist
  FROM stn, pixels p
  WHERE st_dwithin(stn.geom, p.geom, .15)
  ORDER BY dist;

-- SELECT count(*) FROM kuwait_pixels;
-- SELECT count(*) FROM cali_pixels;
SELECT pg_total_relation_size('kuwait_data')/1024^2;
SELECT pg_total_relation_size('cali_data')/1024^2;
SELECT pg_total_relation_size('stn_pix')/1024^2;

-- DROP MATERIALIZED VIEW kuwait_pixels, kuwait_data, cali_pixels, cali_data;
--*/

--/* PERFORMANCE: STORAGE
SELECT pg_size_pretty(pg_database_size('misr'));
-- SELECT pg_size_pretty(pg_total_relation_size('pixels'));
-- SELECT pg_size_pretty(pg_total_relation_size('misr_prods'));
-- SELECT pg_size_pretty(pg_total_relation_size('misr_aux_raw'));
-- SELECT pg_size_pretty(pg_total_relation_size('misr_aux_mix'));

SELECT (pg_total_relation_size('pixels') +
        pg_total_relation_size('misr_prods') +
        pg_total_relation_size('misr_aux_raw') +
        pg_total_relation_size('misr_aux_mix')) / 1024 ^ 3;

SELECT pg_total_relation_size('pixels')/1024^3;
--*/


SELECT * FROM geometry_columns;

select * from test;

--/* USER MANAGEMENT

-- SELECT rolname FROM pg_roles;

/* Best workflow
-- 1 -- Create group and grant privileges to group
CREATE ROLE _group_name_;
GRANT _privileges_ TO _group_name_;
GRANT _group_name_ TO _me_;

-- 2 -- Create user(s) with specified group
CREATE ROLE _user_ WITH PASSWORD _pw_ IN GROUP _group_name_;

-- 3 -- Grant user(s) to myself to get ownership
GRANT _user1_, _user2_ TO _me_;
--*/

-- CREATE ROLE tester;
-- REVOKE ALL PRIVILEGES ON DATABASE misr FROM tester;
-- GRANT SELECT ON TABLE pixels, misr_prods, misr_aux_mix, misr_aux_raw TO tester;
-- GRANT SELECT ON TABLE cali, kuwait TO tester;

-- CREATE ROLE ken WITH LOGIN PASSWORD 'testing12345' IN GROUP tester;
-- CREATE ROLE yaoyi WITH LOGIN PASSWORD 'testing12345' IN GROUP tester;
-- CREATE ROLE michael WITH LOGIN PASSWORD 'testing12345' IN GROUP tester;
-- CREATE ROLE xiaozhe WITH LOGIN PASSWORD 'testing12345' IN GROUP tester;
-- CREATE ROLE lois WITH LOGIN PASSWORD 'testing12345' IN GROUP tester;
-- CREATE ROLE emily WITH LOGIN PASSWORD 'testing12345' IN GROUP tester;
-- CREATE ROLE li WITH LOGIN PASSWORD 'testing12345' IN GROUP tester;
-- CREATE ROLE kate WITH LOGIN PASSWORD 'testing12345' IN GROUP tester;
-- ALTER ROLE ken WITH LOGIN;

-- GRANT tester TO ken;
-- GRANT ken, yaoyi, michael, xiaozhe, lois, emily, li, kate TO kchau;

-- GRANT ALL PRIVILEGES ON DATABASE misr TO kchau;
-- REVOKE ALL PRIVILEGES ON DATABASE misr FROM tester;
-- REVOKE ALL PRIVILEGES ON ALL TABLES IN SCHEMA public FROM tester;
-- ALTER USER user_name WITH PASSWORD 'new_password';

-- SELECT table_catalog, table_schema, table_name, privilege_type
-- FROM   information_schema.table_privileges
-- WHERE  grantee = 'kchau';

--*/


--
-- SELECT pixel_id, count(*) as count
-- FROM misr_prods
-- GROUP BY pixel_id
-- ORDER BY count DESC;


-- SELECT count(*)
-- FROM information_schema.columns
-- WHERE table_name = 'misr_prods';

-- Convex hull
SELECT st_astext(st_convexhull(st_collect(geom)))
FROM pixels
WHERE substr(pixel_id, 1, 3) = 'P04';

-- CREATE MATERIALIZED VIEW cali_data AS
SELECT dat.*
FROM (SELECT pixels.pixel_id
         FROM pixels, cali
         WHERE st_intersects(cali.geom, pixels.geom)) AS p
     JOIN (SELECT * FROM misr_prods JOIN misr_aux_raw
    USING (pixel_id, date_time)) AS dat USING (pixel_id);

SELECT rolname FROM pg_roles;

SELECT *
FROM pixels;
SELECT *
FROM misr_prods;
SELECT *
FROM misr_aux_raw;
SELECT *
FROM misr_aux_mix;

VACUUM ANALYSE pixels;
VACUUM ANALYSE misr_prods;
VACUUM ANALYSE misr_aux_raw;
VACUUM ANALYSE misr_aux_mix;
REINDEX TABLE pixels;
REINDEX TABLE misr_prods;
REINDEX TABLE misr_aux_raw;
REINDEX TABLE misr_aux_mix;

-- TRUNCATE TABLE pixels;
-- TRUNCATE TABLE pixels_temp;
-- TRUNCATE TABLE misr_prods;
-- TRUNCATE TABLE misr_aux_raw;
-- TRUNCATE TABLE misr_aux_mix;

-- DROP TABLE misr_prods;
-- DROP TABLE misr_aux_raw;
-- DROP TABLE misr_aux_mix;

/* re-project cali shapefile to correct SRID
ALTER TABLE cali
  ALTER COLUMN geom
  TYPE geometry(Multipolygon, 4326)
  USING st_transform(geom, 4326);
--*/



-- DROP TABLE stn;
-- CREATE TABLE stn
-- (
--   cbsa_name varchar(23),
--   latitude  numeric,
--   longitude numeric,
--   geom      geometry(Point, 4326)
-- );
-- -- insert data from R
-- UPDATE stn SET geom = st_setsrid(st_makepoint(longitude, latitude), 4326);
-- CREATE INDEX stn_geom_idx
--   ON stn
--   USING gist (geom);

/*
-- DROP TABLE pixels, pixels_temp, misr_prods, misr_aux_raw, misr_aux_mix;

-- Temporary 'loading' table to insert pixels from R, then create geom column
CREATE TABLE pixels_temp
(
  pixel_id char(12) PRIMARY KEY,
  lon      numeric,
  lat      numeric,
  geom     geometry(Point, 4326)
);
-- Table for pixel_id and geometry for spatial filtering
CREATE TABLE pixels
(
  pixel_id char(12) PRIMARY KEY,
  geom     geometry(Point, 4326)
);
CREATE INDEX pixels_geom_idx
  ON pixels
  USING gist (geom);

-- Table for pixel_id and longitude & latitude
-- DROP TABLE pixels;
CREATE TABLE pixels
(
  pixel_id char(12) PRIMARY KEY REFERENCES pixels (pixel_id),
  lon      numeric,
  lat      numeric
);

-- ALTER TABLE pixels_temp
--   ADD COLUMN geom geometry(Point, 4326);
-- UPDATE pixels_temp
-- SET geom = st_setsrid(st_makepoint(lon, lat), 4326);


-- Tables for misr products/raws/mixtures
CREATE TABLE misr_prods
(
  pixel_id         char(12) REFERENCES pixels (pixel_id),
  date_time        timestamp(0),
  elev             integer,
  aod              numeric,
  aod_unc          numeric,
  angs_exp_550_860 numeric,
  absorp_aod       numeric,
  nonsph_aod       numeric,
  small_aod        numeric,
  medium_aod       numeric,
  large_aod        numeric,
  PRIMARY KEY (pixel_id, date_time)
);
-- CREATE INDEX prods_idx
--   ON misr_prods (pixel_id, date_time);

CREATE TABLE misr_aux_raw
(
  pixel_id             char(12) REFERENCES pixels (pixel_id),
  date_time            timestamp(0),
  aod_raw              numeric,
  aod_unc_raw          numeric,
  angs_exp_550_860_raw numeric,
  absorp_aod_raw       numeric,
  nonsph_aod_raw       numeric,
  small_aod_raw        numeric,
  medium_aod_raw       numeric,
  large_aod_raw        numeric,
  aeros_rtrv_conf_idx  numeric,
  cldscrn_param        numeric,
  cldscrn_neighbor3x3  numeric,
  aeros_rtrv_scrn_flag integer,
  col_o3_clim          numeric,
  ocsurf_ws_clim       numeric,
  ocsurf_ws_rtrv       numeric,
  rayleigh_od          numeric,
  lowest_res_mix       integer,
  PRIMARY KEY (pixel_id, date_time)
);
-- CREATE INDEX aux_raw_idx
--   ON misr_aux_raw (pixel_id, date_time);

CREATE TABLE misr_aux_mix
(
  pixel_id     char(12) REFERENCES pixels (pixel_id),
  date_time    timestamp(0),
  aod_mix_01   numeric,
  aod_mix_02   numeric,
  aod_mix_03   numeric,
  aod_mix_04   numeric,
  aod_mix_05   numeric,
  aod_mix_06   numeric,
  aod_mix_07   numeric,
  aod_mix_08   numeric,
  aod_mix_09   numeric,
  aod_mix_10   numeric,
  aod_mix_11   numeric,
  aod_mix_12   numeric,
  aod_mix_13   numeric,
  aod_mix_14   numeric,
  aod_mix_15   numeric,
  aod_mix_16   numeric,
  aod_mix_17   numeric,
  aod_mix_18   numeric,
  aod_mix_19   numeric,
  aod_mix_20   numeric,
  aod_mix_21   numeric,
  aod_mix_22   numeric,
  aod_mix_23   numeric,
  aod_mix_24   numeric,
  aod_mix_25   numeric,
  aod_mix_26   numeric,
  aod_mix_27   numeric,
  aod_mix_28   numeric,
  aod_mix_29   numeric,
  aod_mix_30   numeric,
  aod_mix_31   numeric,
  aod_mix_32   numeric,
  aod_mix_33   numeric,
  aod_mix_34   numeric,
  aod_mix_35   numeric,
  aod_mix_36   numeric,
  aod_mix_37   numeric,
  aod_mix_38   numeric,
  aod_mix_39   numeric,
  aod_mix_40   numeric,
  aod_mix_41   numeric,
  aod_mix_42   numeric,
  aod_mix_43   numeric,
  aod_mix_44   numeric,
  aod_mix_45   numeric,
  aod_mix_46   numeric,
  aod_mix_47   numeric,
  aod_mix_48   numeric,
  aod_mix_49   numeric,
  aod_mix_50   numeric,
  aod_mix_51   numeric,
  aod_mix_52   numeric,
  aod_mix_53   numeric,
  aod_mix_54   numeric,
  aod_mix_55   numeric,
  aod_mix_56   numeric,
  aod_mix_57   numeric,
  aod_mix_58   numeric,
  aod_mix_59   numeric,
  aod_mix_60   numeric,
  aod_mix_61   numeric,
  aod_mix_62   numeric,
  aod_mix_63   numeric,
  aod_mix_64   numeric,
  aod_mix_65   numeric,
  aod_mix_66   numeric,
  aod_mix_67   numeric,
  aod_mix_68   numeric,
  aod_mix_69   numeric,
  aod_mix_70   numeric,
  aod_mix_71   numeric,
  aod_mix_72   numeric,
  aod_mix_73   numeric,
  aod_mix_74   numeric,
  min_chisq_01 numeric,
  min_chisq_02 numeric,
  min_chisq_03 numeric,
  min_chisq_04 numeric,
  min_chisq_05 numeric,
  min_chisq_06 numeric,
  min_chisq_07 numeric,
  min_chisq_08 numeric,
  min_chisq_09 numeric,
  min_chisq_10 numeric,
  min_chisq_11 numeric,
  min_chisq_12 numeric,
  min_chisq_13 numeric,
  min_chisq_14 numeric,
  min_chisq_15 numeric,
  min_chisq_16 numeric,
  min_chisq_17 numeric,
  min_chisq_18 numeric,
  min_chisq_19 numeric,
  min_chisq_20 numeric,
  min_chisq_21 numeric,
  min_chisq_22 numeric,
  min_chisq_23 numeric,
  min_chisq_24 numeric,
  min_chisq_25 numeric,
  min_chisq_26 numeric,
  min_chisq_27 numeric,
  min_chisq_28 numeric,
  min_chisq_29 numeric,
  min_chisq_30 numeric,
  min_chisq_31 numeric,
  min_chisq_32 numeric,
  min_chisq_33 numeric,
  min_chisq_34 numeric,
  min_chisq_35 numeric,
  min_chisq_36 numeric,
  min_chisq_37 numeric,
  min_chisq_38 numeric,
  min_chisq_39 numeric,
  min_chisq_40 numeric,
  min_chisq_41 numeric,
  min_chisq_42 numeric,
  min_chisq_43 numeric,
  min_chisq_44 numeric,
  min_chisq_45 numeric,
  min_chisq_46 numeric,
  min_chisq_47 numeric,
  min_chisq_48 numeric,
  min_chisq_49 numeric,
  min_chisq_50 numeric,
  min_chisq_51 numeric,
  min_chisq_52 numeric,
  min_chisq_53 numeric,
  min_chisq_54 numeric,
  min_chisq_55 numeric,
  min_chisq_56 numeric,
  min_chisq_57 numeric,
  min_chisq_58 numeric,
  min_chisq_59 numeric,
  min_chisq_60 numeric,
  min_chisq_61 numeric,
  min_chisq_62 numeric,
  min_chisq_63 numeric,
  min_chisq_64 numeric,
  min_chisq_65 numeric,
  min_chisq_66 numeric,
  min_chisq_67 numeric,
  min_chisq_68 numeric,
  min_chisq_69 numeric,
  min_chisq_70 numeric,
  min_chisq_71 numeric,
  min_chisq_72 numeric,
  min_chisq_73 numeric,
  min_chisq_74 numeric,
  rtrv_flag_01 numeric,
  rtrv_flag_02 numeric,
  rtrv_flag_03 numeric,
  rtrv_flag_04 numeric,
  rtrv_flag_05 numeric,
  rtrv_flag_06 numeric,
  rtrv_flag_07 numeric,
  rtrv_flag_08 numeric,
  rtrv_flag_09 numeric,
  rtrv_flag_10 numeric,
  rtrv_flag_11 numeric,
  rtrv_flag_12 numeric,
  rtrv_flag_13 numeric,
  rtrv_flag_14 numeric,
  rtrv_flag_15 numeric,
  rtrv_flag_16 numeric,
  rtrv_flag_17 numeric,
  rtrv_flag_18 numeric,
  rtrv_flag_19 numeric,
  rtrv_flag_20 numeric,
  rtrv_flag_21 numeric,
  rtrv_flag_22 numeric,
  rtrv_flag_23 numeric,
  rtrv_flag_24 numeric,
  rtrv_flag_25 numeric,
  rtrv_flag_26 numeric,
  rtrv_flag_27 numeric,
  rtrv_flag_28 numeric,
  rtrv_flag_29 numeric,
  rtrv_flag_30 numeric,
  rtrv_flag_31 numeric,
  rtrv_flag_32 numeric,
  rtrv_flag_33 numeric,
  rtrv_flag_34 numeric,
  rtrv_flag_35 numeric,
  rtrv_flag_36 numeric,
  rtrv_flag_37 numeric,
  rtrv_flag_38 numeric,
  rtrv_flag_39 numeric,
  rtrv_flag_40 numeric,
  rtrv_flag_41 numeric,
  rtrv_flag_42 numeric,
  rtrv_flag_43 numeric,
  rtrv_flag_44 numeric,
  rtrv_flag_45 numeric,
  rtrv_flag_46 numeric,
  rtrv_flag_47 numeric,
  rtrv_flag_48 numeric,
  rtrv_flag_49 numeric,
  rtrv_flag_50 numeric,
  rtrv_flag_51 numeric,
  rtrv_flag_52 numeric,
  rtrv_flag_53 numeric,
  rtrv_flag_54 numeric,
  rtrv_flag_55 numeric,
  rtrv_flag_56 numeric,
  rtrv_flag_57 numeric,
  rtrv_flag_58 numeric,
  rtrv_flag_59 numeric,
  rtrv_flag_60 numeric,
  rtrv_flag_61 numeric,
  rtrv_flag_62 numeric,
  rtrv_flag_63 numeric,
  rtrv_flag_64 numeric,
  rtrv_flag_65 numeric,
  rtrv_flag_66 numeric,
  rtrv_flag_67 numeric,
  rtrv_flag_68 numeric,
  rtrv_flag_69 numeric,
  rtrv_flag_70 numeric,
  rtrv_flag_71 numeric,
  rtrv_flag_72 numeric,
  rtrv_flag_73 numeric,
  rtrv_flag_74 numeric,
  PRIMARY KEY (pixel_id, date_time)
);
-- CREATE INDEX aux_mix_idx
--   ON misr_aux_mix (pixel_id, date_time);
--*/