<?xml version="1.0" encoding="UTF-8"?>
<qgis version="3.28" styleCategories="AllStyleCategories">
  <pipe>
    <rasterrenderer opacity="0.85" alphaBand="-1" band="1" type="singlebandpseudocolor" nodataColor="">
      <rastershader>
        <colorrampshader colorRampType="INTERPOLATED" clip="0" classificationMode="1" labelPrecision="2">
          <item value="0"    color="#ffffff" label="No data"          alpha="0"/>
          <item value="0.05" color="#ffffcc" label="Very Low (0.05)"  alpha="210"/>
          <item value="0.20" color="#fed976" label="Low (0.20)"       alpha="225"/>
          <item value="0.40" color="#fd8d3c" label="Moderate (0.40)"  alpha="235"/>
          <item value="0.65" color="#e31a1c" label="High (0.65)"      alpha="245"/>
          <item value="0.85" color="#bd0026" label="Very High (0.85)" alpha="250"/>
          <item value="1.00" color="#800026" label="Peak (1.00)"      alpha="255"/>
        </colorrampshader>
      </rastershader>
    </rasterrenderer>
    <brightnesscontrast brightness="0" contrast="5" gamma="1"/>
    <huesaturation colorizeOn="0" saturation="15"/>
  </pipe>
  <blendMode>0</blendMode>
  <layerGeometryType>2</layerGeometryType>
</qgis>
