SELECT * FROM geometry_columns;

SELECT * FROM misr_aux_mix;

SELECT * FROM pixels_temp;

SELECT pg_size_pretty(pg_total_relation_size('pixels'));
SELECT pg_size_pretty(pg_total_relation_size('misr_prods'));
SELECT pg_size_pretty(pg_total_relation_size('misr_aux_raw'));
SELECT pg_size_pretty(pg_total_relation_size('misr_aux_mix'));

SELECT pg_size_pretty(pg_database_size('misr'));

SELECT count(*) FROM pixels;
SELECT count(*) FROM misr_prods;

SELECT *
FROM misr_prods;

SELECT extract(MONTH FROM date_time) AS month, count(pixel_id)
FROM misr_prods
GROUP BY month
ORDER BY month;

SELECT * FROM ap;

SELECT DISTINCT *
FROM
  (SELECT pr.pixel_id, pr.date_time, stnpxsub.site_id, stnpxsub.dist,
     pr.aod, pr.small_aod, pr.medium_aod, pr.large_aod,
     pr.nonsph_aod, pr.absorp_aod
   FROM
     misr_prods pr,
     (SELECT site_id, pixel_id, dist
      FROM
        (SELECT ap.site_id, px.pixel_id,
                st_distance(st_transform(ap.geom, 32648),
                            st_transform(px.geom, 32648)) AS dist
         FROM ap, pixels px
         WHERE st_dwithin(ap.geom, px.geom, .1)) stnpx
      WHERE dist <= 10e3) stnpxsub
   WHERE
     pr.pixel_id = stnpxsub.pixel_id
   ORDER BY date_time) aoddata
  JOIN (SELECT mix.*
        FROM
          misr_aux_mix mix,
          (SELECT site_id, pixel_id, dist
           FROM
             (SELECT ap.site_id, px.pixel_id,
                     st_distance(st_transform(ap.geom, 32648),
                                 st_transform(px.geom, 32648)) AS dist
              FROM ap, pixels px
              WHERE st_dwithin(ap.geom, px.geom, .1)) stnpx
           WHERE dist <= 10e3) stnpxsub
        WHERE
          mix.pixel_id = stnpxsub.pixel_id
        ORDER BY date_time) mixdata
       USING (pixel_id, date_time)
ORDER BY site_id, date_time, dist;

SELECT DISTINCT date_time FROM misr_prods
ORDER BY date_time;

SELECT * FROM misr_aux_mix
WHERE date_time = '2010-06-14 03:52:00';



