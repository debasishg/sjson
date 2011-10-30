package sjson.json;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import java.lang.annotation.Retention;

@Retention(RetentionPolicy.RUNTIME)
@Documented
@Inherited
@Target(value = ElementType.METHOD)
public @interface JSONProperty
{
    /**
     * JSON property name to use for this property. if not given, the java property name is used.
     * @return
     */
    String value() default "";

    /**
     * if <code>true</code>, always ignore the property.
     * @return
     */
    boolean ignore() default false;

    /**
     * if <code>true</code>, ignore the property if it contains a <code>null</code> value.
     * @return
     */
    boolean ignoreIfNull() default false;
}
